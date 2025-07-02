#include <iostream>
#include <string>
#include <pugixml.hpp>
#include <curl/curl.h>
#include "jdbc/mysql_driver.h"
#include <jdbc/cppconn/statement.h>
#include <jdbc/cppconn/prepared_statement.h>
#include <jdbc/cppconn/resultset.h>
#include <jdbc/cppconn/exception.h>
#include <windows.h>
#include <sstream>
#include <vector>
#include <boost/tokenizer.hpp>
#include "libstemmer.h"
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <cmath>
#include <set>

// 回调函数：接收 libcurl 数据
size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

// 获取 HTTP 数据
std::string fetch_data_from_url(const std::string& url) {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();

    if (curl) {
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        curl_easy_setopt(curl, CURLOPT_USERAGENT, "Mozilla/5.0 (Windows NT 10.0; Win64; x64)");

        res = curl_easy_perform(curl);
        if (res != CURLE_OK) {
            std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
        }

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();
    return readBuffer;
}

// 将解析的数据存入数据库
void save_to_database(const std::string& title, const std::string& abstract_text, const std::string& authors, const std::string& doi, const std::string& pub_date, const std::string& keywords) {
    try {
        sql::mysql::MySQL_Driver* driver = sql::mysql::get_mysql_driver_instance();
        sql::Connection* con = driver->connect("tcp://localhost:3306", "root", "4321");
        con->setSchema("pubmed_db");

        std::unique_ptr<sql::PreparedStatement> pstmt(con->prepareStatement(
            "INSERT INTO articles (title, abstract, authors, doi, pub_date, keywords) VALUES (?, ?, ?, ?, ?, ?)"));

        pstmt->setString(1, title);
        pstmt->setString(2, abstract_text);
        pstmt->setString(3, authors);
        pstmt->setString(4, doi);
        pstmt->setString(5, pub_date);
        pstmt->setString(6, keywords);

        pstmt->executeUpdate();
        std::cout << "数据已存入数据库：" << title << std::endl;

        delete con;
    }
    catch (sql::SQLException& e) {
        std::cerr << "数据库存储失败：" << e.what() << std::endl;
    }
}

// 解析 PubMed XML 并存入数据库
void parse_xml(const std::string& xml_data) {
    pugi::xml_document doc;
    pugi::xml_parse_result result = doc.load_string(xml_data.c_str());

    if (!result) {
        std::cerr << "XML Parsing error: " << result.description() << std::endl;
        return;
    }

    for (pugi::xml_node entry : doc.child("PubmedArticleSet").children("PubmedArticle")) {
        std::string title, abstract_text, authors, doi, pub_date, keywords;

        // 提取标题
        pugi::xml_node title_node = entry.child("MedlineCitation").child("Article").child("ArticleTitle");
        if (title_node) title = title_node.text().as_string();

        // 提取摘要
        pugi::xml_node abstract_node = entry.child("MedlineCitation").child("Article").child("Abstract");
        if (abstract_node) {
            for (pugi::xml_node abstract_text_node : abstract_node.children("AbstractText")) {
                abstract_text += abstract_text_node.text().as_string() + ' ';
            }
        }

        // 提取作者
        for (pugi::xml_node author : entry.child("MedlineCitation").child("Article").child("AuthorList").children("Author")) {
            pugi::xml_node last_name = author.child("LastName");
            pugi::xml_node first_name = author.child("ForeName");
            if (last_name && first_name) {
                authors += last_name.text().as_string();
                authors += ", " + std::string(first_name.text().as_string()) + "; ";
            }
        }

        // 提取 DOI
        pugi::xml_node doi_node = entry.child("PubmedData").child("ArticleIdList").find_child_by_attribute("ArticleId", "IdType", "doi");
        if (doi_node) doi = doi_node.text().as_string();

        // 提取出版日期
        pugi::xml_node pub_date_node = entry.child("MedlineCitation").child("Article").child("Journal").child("JournalIssue").child("PubDate");
        if (pub_date_node) pub_date = pub_date_node.child("Year").text().as_string();

        // 提取关键词
        for (pugi::xml_node mesh_heading : entry.child("MedlineCitation").child("MeshHeadingList").children("MeshHeading")) {
            pugi::xml_node descriptor = mesh_heading.child("DescriptorName");
            if (descriptor) {
                keywords += descriptor.text().as_string();
                keywords += "; ";
            }
        }

        // 存入数据库
        save_to_database(title, abstract_text, authors, doi, pub_date, keywords);

        // 输出文献信息
        std::cout << "标题: " << title << std::endl;
        std::cout << "摘要: " << abstract_text << std::endl;
        std::cout << "作者: " << authors << std::endl;
        std::cout << "DOI: " << doi << std::endl;
        std::cout << "出版日期: " << pub_date << std::endl;
        std::cout << "关键词: " << keywords << std::endl;
        std::cout << "-------------------------------" << std::endl;
    }
}

// 文本预处理——分词
std::vector<std::string> tokenize(const std::string& text) {
    std::vector<std::string> tokens;
    boost::tokenizer<> tok(text);
    for (auto& word : tok) {
        tokens.push_back(word);
    }
    return tokens;
}

// 文本预处理——词干提取
std::string stem_word(const std::string& word) {
    struct sb_stemmer* stemmer = sb_stemmer_new("english", NULL);
    if (!stemmer) return word;

    const sb_symbol* stemmed = sb_stemmer_stem(stemmer, (const sb_symbol*)word.c_str(), word.size());
    std::string result((const char*)stemmed);

    sb_stemmer_delete(stemmer);
    return result;
}

// 文本预处理——去除停用词
std::unordered_set<std::string> stopwords = { "the", "is", "in", "and", "of", "a", "to", "for", "with", "on", "that", "this" };

bool is_stopword(const std::string& word) {
    return stopwords.find(word) != stopwords.end();
}

std::vector<std::string> remove_stopwords(const std::vector<std::string>& tokens) {
    std::vector<std::string> filtered;
    for (const auto& token : tokens) {
        if (!is_stopword(token)) {
            filtered.push_back(token);
        }
    }
    return filtered;
}

// 布尔检索操作符
enum class BooleanOperator {
    AND,
    OR,
    NOT
};

// 布尔表达式节点
struct BooleanNode {
    std::string term;
    BooleanOperator op;
    std::vector<BooleanNode> children;
};

// 解析布尔表达式的前向声明
BooleanNode parse_boolean_expression(const std::string& query);

// 倒排索引类
class InvertedIndex {
public:
    // 添加文档元数据结构
    struct DocumentMeta {
        std::string title;
        std::string abstract;
        std::string authors;
        std::string doi;
        std::string pub_date;
        std::string keywords;
    };

    // 修改添加文档的方法
    void add_document(const std::string& doc_id, const DocumentMeta& meta) {
        std::cout << "正在处理文档: " << doc_id << std::endl;

        // 处理标题
        std::vector<std::string> title_terms = tokenize(meta.title);
        title_terms = remove_stopwords(title_terms);
        for (auto& term : title_terms) {
            // 转换为小写
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
            if (!term.empty()) {
                index[term].insert(doc_id);
                doc_terms[doc_id].push_back(term);
                std::cout << "索引标题词: " << term << " -> " << doc_id << std::endl;
            }
        }

        // 处理摘要
        std::vector<std::string> abstract_terms = tokenize(meta.abstract);
        abstract_terms = remove_stopwords(abstract_terms);
        for (auto& term : abstract_terms) {
            // 转换为小写
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
            if (!term.empty()) {
                index[term].insert(doc_id);
                doc_terms[doc_id].push_back(term);
                std::cout << "索引摘要词: " << term << " -> " << doc_id << std::endl;
            }
        }

        // 处理关键词
        std::vector<std::string> keyword_terms = tokenize(meta.keywords);
        keyword_terms = remove_stopwords(keyword_terms);
        for (auto& term : keyword_terms) {
            // 转换为小写
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
            if (!term.empty()) {
                index[term].insert(doc_id);
                doc_terms[doc_id].push_back(term);
                std::cout << "索引关键词: " << term << " -> " << doc_id << std::endl;
            }
        }

        // 存储文档元数据
        doc_meta[doc_id] = meta;
        std::cout << "文档 " << doc_id << " 处理完成，共索引 " << doc_terms[doc_id].size() << " 个词" << std::endl;
    }

    // 修改搜索方法，使用enhanced_search
    std::vector<std::pair<std::string, double>> search(const std::string& query) {
        return enhanced_search(query);
    }

    // 获取文档元数据
    DocumentMeta get_document_meta(const std::string& doc_id) const {
        auto it = doc_meta.find(doc_id);
        if (it != doc_meta.end()) {
            return it->second;
        }
        return DocumentMeta();
    }

    // 添加BM25排序函数
    std::vector<std::string> rank_by_bm25(const std::string& term) {
        std::vector<std::pair<std::string, double>> results;
        const double k1 = 1.5;
        const double b = 0.75;

        // 计算平均文档长度
        double avg_doc_length = 0;
        for (const auto& doc : doc_terms) {
            avg_doc_length += doc.second.size();
        }
        avg_doc_length /= doc_terms.size();

        // 计算IDF
        int doc_freq = index[term].size();
        double idf = (doc_freq > 0) ? log((double)doc_terms.size() / doc_freq) : 0;

        // 计算每个文档的BM25分数
        for (const auto& doc : doc_terms) {
            const auto& doc_id = doc.first;
            const auto& terms = doc.second;

            // 计算TF
            int term_count = std::count(terms.begin(), terms.end(), term);
            double tf = (double)term_count / terms.size();

            // 计算文档长度归一化因子
            double doc_length = terms.size();
            double length_norm = (1 - b) + b * (doc_length / avg_doc_length);

            // 计算BM25分数
            double bm25_score = (idf * tf * (k1 + 1)) / (tf + k1 * length_norm);
            results.emplace_back(doc_id, bm25_score);
        }

        // 按分数降序排序
        std::sort(results.begin(), results.end(), [](const auto& a, const auto& b) {
            return a.second > b.second;
            });

        // 只返回文档ID
        std::vector<std::string> doc_ids;
        for (const auto& result : results) {
            doc_ids.push_back(result.first);
        }

        return doc_ids;
    }

    // 短语检索
    std::vector<std::string> phrase_search(const std::vector<std::string>& phrase) {
        std::vector<std::string> results;
        if (phrase.empty()) return results;

        // 获取第一个词的所有文档
        std::unordered_set<std::string> candidate_docs = index[phrase[0]];

        for (const auto& doc_id : candidate_docs) {
            const auto& terms = doc_terms[doc_id];
            bool found = false;

            // 在文档中查找短语
            for (size_t i = 0; i <= terms.size() - phrase.size(); ++i) {
                bool match = true;
                for (size_t j = 0; j < phrase.size(); ++j) {
                    if (terms[i + j] != phrase[j]) {
                        match = false;
                        break;
                    }
                }
                if (match) {
                    found = true;
                    break;
                }
            }

            if (found) {
                results.push_back(doc_id);
            }
        }

        return results;
    }

    // 分页结果
    struct SearchResult {
        std::vector<std::string> doc_ids;
        int total_pages;
        int current_page;
    };

    SearchResult get_paginated_results(const std::vector<std::string>& doc_ids, int page_size, int page_num) {
        SearchResult result;
        int total_docs = doc_ids.size();
        int total_pages = (total_docs + page_size - 1) / page_size;

        // 确保页码有效
        if (page_num < 1) page_num = 1;
        if (page_num > total_pages) page_num = total_pages;

        // 计算当前页的文档
        int start_idx = (page_num - 1) * page_size;
        int end_idx = (start_idx + page_size > total_docs) ? total_docs : start_idx + page_size;

        result.doc_ids.assign(doc_ids.begin() + start_idx, doc_ids.begin() + end_idx);
        result.total_pages = total_pages;
        result.current_page = page_num;

        return result;
    }

    // 关键词高亮
    std::string highlight_keywords(const std::string& text, const std::vector<std::string>& keywords) {
        std::string result = text;
        for (const auto& keyword : keywords) {
            size_t pos = 0;
            while ((pos = result.find(keyword, pos)) != std::string::npos) {
                result.insert(pos, "<mark>");
                result.insert(pos + keyword.length() + 6, "</mark>");
                pos += keyword.length() + 13; // 13 = 6 + 7 (mark tags length)
            }
        }
        return result;
    }

    // 趋势分析
    struct TrendData {
        std::string term;
        std::vector<std::pair<std::string, int>> yearly_counts;
    };

    TrendData analyze_trend(const std::string& term, const std::unordered_map<std::string, std::string>& doc_dates) {
        TrendData trend;
        trend.term = term;

        // 统计每年的出现次数
        std::unordered_map<std::string, int> year_counts;
        for (const auto& doc : index[term]) {
            auto it = doc_dates.find(doc);
            if (it != doc_dates.end()) {
                std::string year = it->second.substr(0, 4);
                year_counts[year]++;
            }
        }

        // 转换为排序后的vector
        for (const auto& pair : year_counts) {
            trend.yearly_counts.push_back(pair);
        }

        std::sort(trend.yearly_counts.begin(), trend.yearly_counts.end(),
            [](const auto& a, const auto& b) { return a.first < b.first; });

        return trend;
    }

    // 词云图数据生成
    struct WordCloudData {
        std::string word;
        int frequency;
    };

    std::vector<WordCloudData> generate_wordcloud_data(const std::string& doc_id) {
        std::vector<WordCloudData> wordcloud;
        std::unordered_map<std::string, int> word_freq;

        // 统计词频
        const auto& terms = doc_terms[doc_id];
        for (const auto& term : terms) {
            word_freq[term]++;
        }

        // 转换为WordCloudData格式
        for (const auto& pair : word_freq) {
            wordcloud.push_back({ pair.first, pair.second });
        }

        // 按频率排序
        std::sort(wordcloud.begin(), wordcloud.end(),
            [](const auto& a, const auto& b) { return a.frequency > b.frequency; });

        return wordcloud;
    }

    void print_index() {
        std::cout << "倒排索引内容：" << std::endl;
        for (const auto& pair : index) {
            std::cout << "术语: " << pair.first << " -> 文档ID: ";
            for (const auto& doc_id : pair.second) {
                std::cout << doc_id << " ";
            }
            std::cout << std::endl; // 换行
        }
    }

    // 执行布尔检索
    std::vector<std::string> execute_boolean_search(const BooleanNode& node) {
        std::vector<std::string> results;

        if (node.children.empty()) {
            // 叶子节点，执行关键词搜索
            return rank_by_bm25(node.term);
        }

        std::vector<std::vector<std::string>> child_results;
        for (const auto& child : node.children) {
            child_results.push_back(execute_boolean_search(child));
        }

        switch (node.op) {
            case BooleanOperator::AND: {
                if (!child_results.empty()) {
                    results = child_results[0];
                    for (size_t i = 1; i < child_results.size(); i++) {
                        std::vector<std::string> intersection;
                        std::set_intersection(results.begin(), results.end(),
                            child_results[i].begin(), child_results[i].end(),
                            std::back_inserter(intersection));
                        results = intersection;
                    }
                }
                break;
            }
            case BooleanOperator::OR: {
                std::set<std::string> union_set;
                for (const auto& child_result : child_results) {
                    union_set.insert(child_result.begin(), child_result.end());
                }
                results.assign(union_set.begin(), union_set.end());
                break;
            }
            case BooleanOperator::NOT: {
                if (child_results.size() == 2) {
                    std::set<std::string> all_docs;
                    for (const auto& pair : doc_meta) {
                        all_docs.insert(pair.first);
                    }
                    std::set<std::string> exclude_docs(child_results[1].begin(), child_results[1].end());
                    std::set_difference(all_docs.begin(), all_docs.end(),
                        exclude_docs.begin(), exclude_docs.end(),
                        std::back_inserter(results));
                }
                break;
            }
        }

        return results;
    }

    // 修改BM25分数计算函数
    double calculate_bm25_score(const std::string& doc_id, const std::string& term) {
        const double k1 = 1.5;
        const double b = 0.75;

        // 计算平均文档长度
        double avg_doc_length = 0;
        for (const auto& doc : doc_terms) {
            avg_doc_length += doc.second.size();
        }
        if (doc_terms.empty()) return 0.0;
        avg_doc_length /= doc_terms.size();

        // 计算IDF
        int doc_freq = index[term].size();
        if (doc_freq == 0) return 0.0;
        // 修改IDF计算，使用更标准的公式
        double idf = log((double)(doc_terms.size() - doc_freq + 0.5) / (doc_freq + 0.5) + 1.0);

        // 计算TF
        const auto& terms = doc_terms[doc_id];
        if (terms.empty()) return 0.0;
        int term_count = std::count(terms.begin(), terms.end(), term);
        if (term_count == 0) return 0.0;

        // 计算TF，考虑词的位置权重
        double tf = 0.0;
        for (size_t i = 0; i < terms.size(); ++i) {
            if (terms[i] == term) {
                // 标题中的词权重更高
                if (i < 10) {  // 假设前10个词是标题
                    tf += 2.0;
                }
                else {
                    tf += 1.0;
                }
            }
        }
        tf /= terms.size();

        // 计算文档长度归一化因子
        double doc_length = terms.size();
        double length_norm = (1 - b) + b * (doc_length / avg_doc_length);

        // 计算BM25分数
        double score = (idf * tf * (k1 + 1)) / (tf + k1 * length_norm);

        // 添加调试信息
        std::cout << "文档 " << doc_id << " 的词 " << term << " 的BM25计算：" << std::endl;
        std::cout << "  - 词频(TF): " << tf << std::endl;
        std::cout << "  - 逆文档频率(IDF): " << idf << std::endl;
        std::cout << "  - 文档长度归一化: " << length_norm << std::endl;
        std::cout << "  - 最终得分: " << score << std::endl;

        return score;
    }

    // 修改enhanced_search函数
    std::vector<std::pair<std::string, double>> enhanced_search(const std::string& query) {
        // 预处理查询词
        std::vector<std::string> query_terms = tokenize(query);
        query_terms = remove_stopwords(query_terms);
        for (auto& term : query_terms) {
            // 转换为小写
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
        }

        std::cout << "处理后的查询词: ";
        for (const auto& term : query_terms) {
            std::cout << term << " ";
        }
        std::cout << std::endl;

        if (query_terms.empty()) {
            return std::vector<std::pair<std::string, double>>();
        }

        // 检查是否是布尔查询
        if (query.find(" AND ") != std::string::npos ||
            query.find(" OR ") != std::string::npos ||
            query.find(" NOT ") != std::string::npos) {
            BooleanNode root = parse_boolean_expression(query);
            std::vector<std::string> doc_ids = execute_boolean_search(root);

            std::vector<std::pair<std::string, double>> ranked_results;
            for (const auto& doc_id : doc_ids) {
                double score = 0.0;
                for (const auto& term : query_terms) {
                    if (term != "and" && term != "or" && term != "not") {
                        score += calculate_bm25_score(doc_id, term);
                    }
                }
                if (score > 0) {
                    ranked_results.push_back({ doc_id, score });
                }
            }

            std::sort(ranked_results.begin(), ranked_results.end(),
                [](const auto& a, const auto& b) { return a.second > b.second; });
            return ranked_results;
        }

        // 检查是否是短语查询
        if (query.find("\"") != std::string::npos) {
            std::string phrase = query.substr(1, query.length() - 2);
            std::vector<std::string> phrase_terms = tokenize(phrase);
            phrase_terms = remove_stopwords(phrase_terms);
            for (auto& term : phrase_terms) {
                std::transform(term.begin(), term.end(), term.begin(), ::tolower);
                term = stem_word(term);
            }
            std::vector<std::string> doc_ids = phrase_search(phrase_terms);

            std::vector<std::pair<std::string, double>> ranked_results;
            for (const auto& doc_id : doc_ids) {
                double score = 0.0;
                for (const auto& term : phrase_terms) {
                    score += calculate_bm25_score(doc_id, term);
                }
                if (score > 0) {
                    ranked_results.push_back({ doc_id, score });
                }
            }

            std::sort(ranked_results.begin(), ranked_results.end(),
                [](const auto& a, const auto& b) { return a.second > b.second; });
            return ranked_results;
        }

        // 普通关键词查询
        std::unordered_set<std::string> candidate_docs;
        for (const auto& term : query_terms) {
            auto it = index.find(term);
            if (it != index.end()) {
                std::cout << "找到词 " << term << " 的文档: ";
                for (const auto& doc_id : it->second) {
                    std::cout << doc_id << " ";
                }
                std::cout << std::endl;
                candidate_docs.insert(it->second.begin(), it->second.end());
            }
            else {
                std::cout << "未找到词 " << term << " 的文档" << std::endl;
            }
        }

        std::vector<std::pair<std::string, double>> ranked_results;
        for (const auto& doc_id : candidate_docs) {
            double score = 0.0;
            for (const auto& term : query_terms) {
                double term_score = calculate_bm25_score(doc_id, term);
                score += term_score;
                std::cout << "文档 " << doc_id << " 的词 " << term << " 得分: " << term_score << std::endl;
            }
            if (score > 0) {
                ranked_results.push_back({ doc_id, score });
                std::cout << "文档 " << doc_id << " 的总得分: " << score << std::endl;
            }
        }

        std::sort(ranked_results.begin(), ranked_results.end(),
            [](const auto& a, const auto& b) { return a.second > b.second; });
        return ranked_results;
    }

private:
    std::unordered_map<std::string, std::unordered_set<std::string>> index;
    std::unordered_map<std::string, std::vector<std::string>> doc_terms;
    std::unordered_map<std::string, DocumentMeta> doc_meta;
};

// 解析布尔表达式
BooleanNode parse_boolean_expression(const std::string& query) {
    BooleanNode root;
    std::vector<std::string> tokens = tokenize(query);

    for (size_t i = 0; i < tokens.size(); i++) {
        if (tokens[i] == "AND") {
            root.op = BooleanOperator::AND;
        }
        else if (tokens[i] == "OR") {
            root.op = BooleanOperator::OR;
        }
        else if (tokens[i] == "NOT") {
            root.op = BooleanOperator::NOT;
        }
        else {
            BooleanNode term_node;
            term_node.term = tokens[i];
            root.children.push_back(term_node);
        }
    }

    return root;
}

// 主函数
int main() {
    std::string query;  // 用户输入的查询关键词
    int max_results = 20;  // 返回的最大文献数，默认 20
    int search_type;  // 检索方式选择

    // 提示用户选择检索方式
    std::cout << "请选择检索方式：" << std::endl;
    std::cout << "1. 关键词检索（直接输入关键词）" << std::endl;
    std::cout << "2. 布尔检索（使用 AND、OR、NOT，如：cancer AND treatment）" << std::endl;
    std::cout << "3. 短语检索（使用双引号，如：\"cancer treatment\"）" << std::endl;
    std::cout << "请输入检索方式（1-3）：";
    std::cin >> search_type;
    std::cin.ignore();  // 忽略回车符

    // 验证检索方式选择
    if (search_type < 1 || search_type > 3) {
        std::cerr << "错误：无效的检索方式选择！" << std::endl;
        return 1;
    }

    // 根据检索方式提示用户输入
    switch (search_type) {
        case 1:
            std::cout << "请输入关键词（如：cancer）：";
            break;
        case 2:
            std::cout << "请输入布尔表达式（如：cancer AND treatment）：";
            break;
        case 3:
            std::cout << "请输入短语（用双引号包围，如：\"cancer treatment\"）：";
            break;
    }
    std::getline(std::cin, query);

    // 提示用户输入返回的最大文献数
    std::cout << "请输入返回的最大文献数（默认 20，输入 0 则不限制）：";
    std::cin >> max_results;  // 读取用户输入的最大文献数
    std::cin.ignore();  // 忽略回车符，确保后续输入正常

    // 如果查询关键词为空，输出错误信息并退出
    if (query.empty()) {
        std::cerr << "错误：查询关键词不能为空！" << std::endl;
        return 1;
    }

    // 验证检索输入格式
    switch (search_type) {
        case 2:  // 布尔检索
            if (query.find(" AND ") == std::string::npos &&
                query.find(" OR ") == std::string::npos &&
                query.find(" NOT ") == std::string::npos) {
                std::cerr << "错误：布尔检索必须包含 AND、OR 或 NOT 操作符！" << std::endl;
                return 1;
            }
            break;
        case 3:  // 短语检索
            if (query.front() != '"' || query.back() != '"') {
                std::cerr << "错误：短语检索必须用双引号包围！" << std::endl;
                return 1;
            }
            break;
    }

    // 使用 CURL 对查询关键词进行 URL 编码
    CURL* curl = curl_easy_init();  // 初始化 CURL 对象
    char* encoded_query = curl_easy_escape(curl, query.c_str(), query.length());  // URL 编码
    std::string query_param = encoded_query ? encoded_query : query;  // 如果编码成功，使用编码后的查询词，否则使用原查询词
    curl_free(encoded_query);  // 释放编码后的字符串
    curl_easy_cleanup(curl);  // 清理 CURL 对象

    // 构建 PubMed 查询的 URL
    std::string esearch_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=" + query_param + "&retmode=xml";
    if (max_results > 0) {
        esearch_url += "&retmax=" + std::to_string(max_results);  // 如果用户输入的最大文献数大于 0，添加 retmax 参数
    }

    // 获取 PubMed 查询结果的 XML 数据
    std::string xml_search_result = fetch_data_from_url(esearch_url);  // 从指定 URL 获取数据
    pugi::xml_document search_doc;  // 创建 XML 文档对象
    search_doc.load_string(xml_search_result.c_str());  // 将获取的 XML 数据加载到文档中

    // 提取文献 ID 列表
    std::string id_list;
    for (pugi::xml_node id_node : search_doc.child("eSearchResult").child("IdList").children("Id")) {
        id_list += id_node.text().as_string();  // 获取每个文献的 ID
        id_list += ",";  // 用逗号分隔 ID
    }
    if (!id_list.empty()) id_list.pop_back();  // 移除最后一个逗号

    // 如果没有找到相关文献，输出提示并退出
    if (id_list.empty()) {
        std::cerr << "未找到相关文献，请尝试其他关键词。" << std::endl;
        return 1;
    }

    // 构建 PubMed 获取文献详细信息的 URL
    std::string efetch_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=" + id_list + "&retmode=xml";
    std::string xml_data = fetch_data_from_url(efetch_url);  // 获取详细文献数据

    // 解析 XML 数据并输出文献信息
    parse_xml(xml_data);

    // 构建倒排索引
    InvertedIndex index;
    pugi::xml_document doc;
    doc.load_string(xml_data.c_str());  // 使用获取到的详细文献数据

    for (pugi::xml_node entry : doc.child("PubmedArticleSet").children("PubmedArticle")) {
        std::string doc_id;
        pugi::xml_node id_node = entry.child("PubmedData").child("ArticleIdList").child("ArticleId");
        if (id_node) doc_id = id_node.text().as_string();

        InvertedIndex::DocumentMeta meta;

        // 提取标题
        pugi::xml_node title_node = entry.child("MedlineCitation").child("Article").child("ArticleTitle");
        if (title_node) meta.title = title_node.text().as_string();

        // 提取摘要
        pugi::xml_node abstract_node = entry.child("MedlineCitation").child("Article").child("Abstract");
        if (abstract_node) {
            for (pugi::xml_node abstract_text_node : abstract_node.children("AbstractText")) {
                meta.abstract += abstract_text_node.text().as_string() + ' ';
            }
        }

        // 提取作者
        for (pugi::xml_node author : entry.child("MedlineCitation").child("Article").child("AuthorList").children("Author")) {
            pugi::xml_node last_name = author.child("LastName");
            pugi::xml_node first_name = author.child("ForeName");
            if (last_name && first_name) {
                meta.authors += last_name.text().as_string();
                meta.authors += ", " + std::string(first_name.text().as_string()) + "; ";
            }
        }

        // 提取DOI
        pugi::xml_node doi_node = entry.child("PubmedData").child("ArticleIdList").find_child_by_attribute("ArticleId", "IdType", "doi");
        if (doi_node) meta.doi = doi_node.text().as_string();

        // 提取出版日期
        pugi::xml_node pub_date_node = entry.child("MedlineCitation").child("Article").child("Journal").child("JournalIssue").child("PubDate");
        if (pub_date_node) meta.pub_date = pub_date_node.child("Year").text().as_string();

        // 提取关键词
        for (pugi::xml_node mesh_heading : entry.child("MedlineCitation").child("MeshHeadingList").children("MeshHeading")) {
            pugi::xml_node descriptor = mesh_heading.child("DescriptorName");
            if (descriptor) {
                meta.keywords += descriptor.text().as_string();
                meta.keywords += "; ";
            }
        }

        // 添加到倒排索引
        index.add_document(doc_id, meta);
    }

    // 执行检索
    std::cout << "\n执行检索：" << std::endl;
    std::cout << "检索方式：" << (search_type == 1 ? "关键词检索" :
        search_type == 2 ? "布尔检索" : "短语检索") << std::endl;
    std::cout << "检索词：" << query << std::endl;
    auto search_results = index.enhanced_search(query);

    std::cout << "\n检索结果：" << std::endl;
    if (search_results.empty()) {
        std::cout << "未找到相关文献。" << std::endl;
    }
    else {
        std::cout << "找到 " << search_results.size() << " 篇相关文献：" << std::endl;
        for (const auto& result : search_results) {
            const auto& doc_id = result.first;
            double score = result.second;
            const auto& meta = index.get_document_meta(doc_id);

            std::cout << "\n文献ID: " << doc_id << std::endl;
            std::cout << "标题: " << meta.title << std::endl;
            std::cout << "作者: " << meta.authors << std::endl;
            std::cout << "出版日期: " << meta.pub_date << std::endl;
            std::cout << "相关度得分: " << score << std::endl;
            std::cout << "-------------------------------" << std::endl;
        }
    }

    return 0;  // 程序结束
}
