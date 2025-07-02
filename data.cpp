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
#pragma execution_character_set("utf-8")
using namespace std;

// 设置控制台输出编码为UTF-8
void SetUTF8Output() {
    SetConsoleOutputCP(CP_UTF8);
    system("chcp 65001");
}

// 回调函数：接收 libcurl 数据
size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

// 从URL获取HTTP数据
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
            std::cerr << "curl_easy_perform() execution failed: " << curl_easy_strerror(res) << std::endl;
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
        cout << "Data saved to database: " << title << endl;

        delete con;
    }
    catch (sql::SQLException& e) {
        cerr << "Database error: " << e.what() << endl;
    }
}

// 解析PubMed XML并存入数据库
void parse_xml(const std::string& xml_data) {
    pugi::xml_document doc;
    pugi::xml_parse_result result = doc.load_string(xml_data.c_str());

    if (!result) {
        std::cerr << "XML parsing error: " << result.description() << std::endl;
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

        // 提取DOI
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
        std::cout << "Title: " << title << std::endl;
        std::cout << "Abstract: " << abstract_text << std::endl;
        std::cout << "Authors: " << authors << std::endl;
        std::cout << "DOI: " << doi << std::endl;
        std::cout << "Publication Date: " << pub_date << std::endl;
        std::cout << "Keywords: " << keywords << std::endl;
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

// 文本预处理——停用词列表
std::unordered_set<std::string> stopwords = { "the", "is", "in", "and", "of", "a", "to", "for", "with", "on", "that", "this" };

// 判断是否为停用词
bool is_stopword(const std::string& word) {
    return stopwords.find(word) != stopwords.end();
}

// 移除停用词
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
    AND,  // 与操作
    OR,   // 或操作
    NOT   // 非操作
};

// 布尔表达式节点结构
struct BooleanNode {
    std::string term;              // 检索词
    BooleanOperator op;           // 操作符
    std::vector<BooleanNode> children;  // 子节点
};

// 解析布尔表达式的前向声明
BooleanNode parse_boolean_expression(const std::string& query);

// 倒排索引类
class InvertedIndex {
public:
    // 文档元数据结构
    struct DocumentMeta {
        std::string title;       // 标题
        std::string abstract;    // 摘要
        std::string authors;     // 作者
        std::string doi;         // DOI标识符
        std::string pub_date;    // 出版日期
        std::string keywords;    // 关键词
    };

    // 添加文档到索引
    void add_document(const std::string& doc_id, const DocumentMeta& meta) {
        // 处理标题
        std::vector<std::string> title_terms = tokenize(meta.title);
        title_terms = remove_stopwords(title_terms);
        for (auto& term : title_terms) {
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
            if (!term.empty()) {
                index[term].insert(doc_id);
                doc_terms[doc_id].push_back(term);
            }
        }

        // 处理摘要
        std::vector<std::string> abstract_terms = tokenize(meta.abstract);
        abstract_terms = remove_stopwords(abstract_terms);
        for (auto& term : abstract_terms) {
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
            if (!term.empty()) {
                index[term].insert(doc_id);
                doc_terms[doc_id].push_back(term);
            }
        }

        // 处理关键词
        std::vector<std::string> keyword_terms = tokenize(meta.keywords);
        keyword_terms = remove_stopwords(keyword_terms);
        for (auto& term : keyword_terms) {
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
            if (!term.empty()) {
                index[term].insert(doc_id);
                doc_terms[doc_id].push_back(term);
            }
        }

        doc_meta[doc_id] = meta;
    }

    // 基本搜索方法
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

    // BM25排序实现
    std::vector<std::string> rank_by_bm25(const std::string& term) {
        std::vector<std::pair<std::string, double>> results;
        const double k1 = 1.5;  // BM25参数k1
        const double b = 0.75;   // BM25参数b

        // 计算平均文档长度
        double avg_doc_length = 0;
        for (const auto& doc : doc_terms) {
            avg_doc_length += doc.second.size();
        }
        avg_doc_length /= doc_terms.size();

        // 计算IDF（逆文档频率）
        int doc_freq = index[term].size();
        double idf = (doc_freq > 0) ? log((double)doc_terms.size() / doc_freq) : 0;

        // 计算每个文档的BM25分数
        for (const auto& doc : doc_terms) {
            const auto& doc_id = doc.first;
            const auto& terms = doc.second;

            // 计算TF（词频）
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

        // 提取文档ID列表
        std::vector<std::string> doc_ids;
        for (const auto& result : results) {
            doc_ids.push_back(result.first);
        }

        return doc_ids;
    }

    // 短语检索实现
    std::vector<std::string> phrase_search(const std::vector<std::string>& phrase) {
        std::vector<std::string> results;
        if (phrase.empty()) return results;

        // 获取第一个词的所有文档
        std::unordered_set<std::string> candidate_docs = index[phrase[0]];

        // 在候选文档中查找完整短语
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

    // 分页结果结构
    struct SearchResult {
        std::vector<std::string> doc_ids;     // 文档ID列表
        int total_pages;                      // 总页数
        int current_page;                     // 当前页码
    };

    // 获取分页结果
    SearchResult get_paginated_results(const std::vector<std::string>& doc_ids, int page_size, int page_num) {
        SearchResult result;
        int total_docs = doc_ids.size();
        int total_pages = (total_docs + page_size - 1) / page_size;

        // 确保页码有效
        if (page_num < 1) page_num = 1;
        if (page_num > total_pages) page_num = total_pages;

        // 计算当前页的文档范围
        int start_idx = (page_num - 1) * page_size;
        int end_idx = (start_idx + page_size > total_docs) ? total_docs : start_idx + page_size;

        result.doc_ids.assign(doc_ids.begin() + start_idx, doc_ids.begin() + end_idx);
        result.total_pages = total_pages;
        result.current_page = page_num;

        return result;
    }

    // 关键词高亮处理
    std::string highlight_keywords(const std::string& text, const std::vector<std::string>& keywords) {
        std::string result = text;
        const std::string highlight_start = "\033[30;43m";  // 黑色字体(30)，黄色背景(43)
        const std::string highlight_end = "\033[0m";        // 恢复默认样式

        for (const auto& keyword : keywords) {
            std::string lower_text = result;
            std::string lower_keyword = keyword;
            
            // 转换为小写进行比较
            std::transform(lower_text.begin(), lower_text.end(), lower_text.begin(), ::tolower);
            std::transform(lower_keyword.begin(), lower_keyword.end(), lower_keyword.begin(), ::tolower);

            size_t pos = 0;
            size_t offset = 0;
            
            // 查找并高亮所有匹配项
            while ((pos = lower_text.find(lower_keyword, pos)) != std::string::npos) {
                result.insert(pos + offset, highlight_start);
                offset += highlight_start.length();
                
                result.insert(pos + offset + keyword.length(), highlight_end);
                offset += highlight_end.length();
                
                pos += keyword.length();
            }
        }
        return result;
    }

    // 趋势分析数据结构
    struct TrendData {
        std::string term;                                    // 检索词
        std::vector<std::pair<std::string, int>> yearly_counts;  // 年度统计
    };

    // 执行趋势分析
    TrendData analyze_trend(const std::string& term) const {
        TrendData trend;
        trend.term = term;

        // 统计每年的出现次数
        std::unordered_map<std::string, int> year_counts;
        auto it = index.find(term);
        if (it != index.end()) {
            for (const auto& doc_id : it->second) {
                auto meta_it = doc_meta.find(doc_id);
                if (meta_it != doc_meta.end()) {
                    const std::string& pub_date = meta_it->second.pub_date;
                    if (!pub_date.empty()) {
                        std::string year = pub_date.substr(0, 4);
                        year_counts[year]++;
                    }
                }
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

    // 词云数据结构
    struct WordCloudData {
        std::string word;    // 词语
        int frequency;       // 频率
    };

    // 生成词云数据
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

        // 按频率降序排序
        std::sort(wordcloud.begin(), wordcloud.end(),
            [](const auto& a, const auto& b) { return a.frequency > b.frequency; });

        return wordcloud;
    }

    // 打印索引内容
    void print_index() {
        cout << "Index Contents:" << endl;
        for (const auto& pair : index) {
            cout << "Term: " << pair.first << " -> Document IDs: ";
            for (const auto& doc_id : pair.second) {
                cout << doc_id << " ";
            }
            cout << endl;
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

        // 修改调试信息
        std::cout << "BM25 calculation for document " << doc_id << " term " << term << ":\n"
                  << "  - Term frequency (TF): " << tf << "\n"
                  << "  - Inverse document frequency (IDF): " << idf << "\n"
                  << "  - Document length normalization: " << length_norm << "\n"
                  << "  - Final score: " << score << std::endl;

        return score;
    }

    // 修改enhanced_search函数
    std::vector<std::pair<std::string, double>> enhanced_search(const std::string& query) {
        std::vector<std::string> query_terms = tokenize(query);
        query_terms = remove_stopwords(query_terms);
        for (auto& term : query_terms) {
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
        }

        std::cout << "Processed query terms: ";
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
                cout << "Term found in document: " << term << endl;  // Changed to English output
                candidate_docs.insert(it->second.begin(), it->second.end());
            }
            else {
                cout << "Term not found in any document: " << term << endl;  // Changed to English output
            }
        }

        std::vector<std::pair<std::string, double>> ranked_results;
        for (const auto& doc_id : candidate_docs) {
            double score = 0.0;
            for (const auto& term : query_terms) {
                double term_score = calculate_bm25_score(doc_id, term);
                score += term_score;
                std::cout << "Document " << doc_id << " term " << term << " score: " << term_score << std::endl;
            }
            if (score > 0) {
                ranked_results.push_back({ doc_id, score });
                std::cout << "Document " << doc_id << " total score: " << score << std::endl;
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
// 分页功能实现
struct PaginationResult {
    std::vector<std::pair<std::string, double>> results;
    int total_pages;
    int current_page;
    int page_size;
};

PaginationResult get_paginated_results(const std::vector<std::pair<std::string, double>>& all_results, int page_size, int page_num) {
    PaginationResult result;
    result.page_size = page_size;
    result.total_pages = (all_results.size() + page_size - 1) / page_size;

    // 确保页码有效
    if (page_num < 1) page_num = 1;
    if (page_num > result.total_pages) page_num = result.total_pages;
    result.current_page = page_num;

    // 计算当前页的结果
    int start_idx = (page_num - 1) * page_size;
    int end_idx = min(start_idx + page_size, static_cast<int>(all_results.size()));

    result.results.assign(all_results.begin() + start_idx, all_results.begin() + end_idx);
    return result;
}

// 关键词高亮功能实现
std::string highlight_keywords(const std::string& text, const std::vector<std::string>& keywords) {
    std::string result = text;
    const std::string highlight_start = "\033[30;43m";  // 黑色字体(30)，黄色背景(43)
    const std::string highlight_end = "\033[0m";     // 恢复默认样式

    for (const auto& keyword : keywords) {
        std::string lower_text = result;
        std::string lower_keyword = keyword;
        
        // 转换为小写进行比较
        std::transform(lower_text.begin(), lower_text.end(), lower_text.begin(), ::tolower);
        std::transform(lower_keyword.begin(), lower_keyword.end(), lower_keyword.begin(), ::tolower);

        size_t pos = 0;
        size_t offset = 0;
        
        // 查找所有匹配项并高亮
        while ((pos = lower_text.find(lower_keyword, pos)) != std::string::npos) {
            // 在原始文本中插入高亮标记
            result.insert(pos + offset, highlight_start);
            offset += highlight_start.length();
            
            result.insert(pos + offset + keyword.length(), highlight_end);
            offset += highlight_end.length();
            
            pos += keyword.length();
        }
    }
    return result;
}

// 定义数据库类型枚举
enum class DatabaseType {
    PUBMED,
    PMC
};

// 从PMC获取数据的函数
std::string fetch_pmc_data(const std::string& pmc_id) {
    std::string pmc_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&id=" + pmc_id + "&retmode=xml";
    return fetch_data_from_url(pmc_url);
}

// 解析PMC XML的函数
void parse_pmc_xml(const std::string& xml_data) {
    pugi::xml_document doc;
    pugi::xml_parse_result result = doc.load_string(xml_data.c_str());

    if (!result) {
        std::cerr << "XML parsing error: " << result.description() << std::endl;
        return;
    }

    for (pugi::xml_node article : doc.child("pmc-articleset").children("article")) {
        std::string title, abstract_text, authors, doi, pub_date, keywords;

        // 提取标题 (PMC格式)
        pugi::xml_node title_node = article.select_node(".//article-title").node();
        if (title_node) title = title_node.text().as_string();

        // 提取摘要 (PMC格式)
        pugi::xml_node abstract_node = article.select_node(".//abstract").node();
        if (abstract_node) {
            for (pugi::xml_node p : abstract_node.children("p")) {
                abstract_text += p.text().as_string() + ' ';
            }
        }

        // 提取作者 (PMC格式)
        pugi::xml_node contrib_group = article.select_node(".//contrib-group").node();
        if (contrib_group) {
            for (pugi::xml_node contrib : contrib_group.children("contrib")) {
                if (std::string(contrib.attribute("contrib-type").value()) == "author") {
                    pugi::xml_node name = contrib.child("name");
                    if (name) {
                        pugi::xml_node surname = name.child("surname");
                        pugi::xml_node given_names = name.child("given-names");
                        if (surname && given_names) {
                            authors += surname.text().as_string();
                            authors += ", " + std::string(given_names.text().as_string()) + "; ";
                        }
                    }
                }
            }
        }

        // 提取DOI (PMC格式)
        pugi::xml_node doi_node = article.select_node(".//article-id[@pub-id-type='doi']").node();
        if (doi_node) doi = doi_node.text().as_string();

        // 提取出版日期 (PMC格式)
        pugi::xml_node pub_date_node = article.select_node(".//pub-date[@pub-type='epub']").node();
        if (!pub_date_node) {
            // 如果没有epub日期，尝试获取其他类型的出版日期
            pub_date_node = article.select_node(".//pub-date").node();
        }
        if (pub_date_node) {
            pugi::xml_node year = pub_date_node.child("year");
            if (year) {
                pub_date = year.text().as_string();
            }
        }

        // 提取关键词 (PMC格式)
        pugi::xml_node kwd_group = article.select_node(".//kwd-group").node();
        if (kwd_group) {
            for (pugi::xml_node kwd : kwd_group.children("kwd")) {
                keywords += kwd.text().as_string() + ';';
            }
        }

        // 存入数据库
        save_to_database(title, abstract_text, authors, doi, pub_date, keywords);
    }
}

// 主函数
int main() {
    SetUTF8Output();  // 设置UTF-8输出
    
    // 启用Windows控制台的ANSI转义序列支持
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD dwMode = 0;
    GetConsoleMode(hOut, &dwMode);
    dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    SetConsoleMode(hOut, dwMode);
    
    std::string query;              // 用户输入的查询关键词
    int max_results = 20;          // 返回的最大文献数
    int search_type;               // 检索方式选择
    
    // 定义用于趋势分析的关键词列表
    std::vector<std::string> highlight_terms;

    // 提示用户选择检索方式
    cout << "Please select search type:" << endl
         << "1. Keyword search (directly input keywords)" << endl
         << "2. Boolean search (using AND, OR, NOT, e.g.: cancer AND treatment)" << endl
         << "3. Phrase search (using quotes, e.g.: \"cancer treatment\")" << endl
         << "Please enter search type (1-3): ";
    std::cin >> search_type;
    std::cin.ignore();  // 忽略回车符

    // 验证检索方式选择
    if (search_type < 1 || search_type > 3) {
        std::cerr << "Error: Invalid search type selection!" << std::endl;
        return 1;
    }

    // 根据检索方式提示用户输入
    switch (search_type) {
        case 1:
            std::cout << "Please enter keywords (e.g.: cancer): ";
            break;
        case 2:
            std::cout << "Please enter boolean expression (e.g.: cancer AND treatment): ";
            break;
        case 3:
            std::cout << "Please enter phrase (in quotes, e.g.: \"cancer treatment\"): ";
            break;
    }
    std::getline(std::cin, query);

    // 提示用户输入返回的最大文献数
    std::cout << "Please enter maximum number of results (default 20): ";
    std::cin >> max_results;
    std::cin.ignore();

    // 检查查询关键词是否为空
    if (query.empty()) {
        std::cerr << "Error: Search query cannot be empty!" << std::endl;
        return 1;
    }

    // 验证检索输入格式
    switch (search_type) {
        case 2:  // 布尔检索
            if (query.find(" AND ") == std::string::npos &&
                query.find(" OR ") == std::string::npos &&
                query.find(" NOT ") == std::string::npos) {
                std::cerr << "Error: Boolean search must include AND, OR, or NOT operators!" << std::endl;
                return 1;
            }
            break;
        case 3:  // 短语检索
            if (query.front() != '"' || query.back() != '"') {
                std::cerr << "Error: Phrase search must be enclosed in quotes!" << std::endl;
                return 1;
            }
            break;
    }

    // 使用CURL进行URL编码
    CURL* curl = curl_easy_init();
    char* encoded_query = curl_easy_escape(curl, query.c_str(), query.length());
    std::string query_param = encoded_query ? encoded_query : query;
    curl_free(encoded_query);
    curl_easy_cleanup(curl);

    // 添加数据库选择
    DatabaseType db_type;
    std::cout << "Select database:" << std::endl
              << "1. PubMed" << std::endl
              << "2. PMC" << std::endl
              << "Please enter database type (1-2): ";
    int db_choice;
    std::cin >> db_choice;
    std::cin.ignore();

    db_type = (db_choice == 1) ? DatabaseType::PUBMED : DatabaseType::PMC;

    // 根据选择的数据库构建URL
    std::string search_url;
    if (db_type == DatabaseType::PUBMED) {
        search_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=" + query_param + "&retmode=xml";
    } else {
        search_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pmc&term=" + query_param + "&retmode=xml";
    }

    if (max_results > 0) {
        search_url += "&retmax=" + std::to_string(max_results);
    }

    // 获取检索结果
    std::string xml_search_result = fetch_data_from_url(search_url);
    pugi::xml_document search_doc;
    search_doc.load_string(xml_search_result.c_str());

    // 提取文献ID列表
    std::string id_list;
    for (pugi::xml_node id_node : search_doc.child("eSearchResult").child("IdList").children("Id")) {
        id_list += id_node.text().as_string();
        id_list += ",";
    }
    if (!id_list.empty()) id_list.pop_back();

    // 根据数据库类型获取详细信息
    std::string xml_data;
    if (db_type == DatabaseType::PUBMED) {
        std::string efetch_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=" + id_list + "&retmode=xml";
        xml_data = fetch_data_from_url(efetch_url);
        parse_xml(xml_data);
    } else {
        std::string efetch_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&id=" + id_list + "&retmode=xml";
        xml_data = fetch_data_from_url(efetch_url);
        parse_pmc_xml(xml_data);
    }

    // 解析XML数据并构建索引
    InvertedIndex index;
    pugi::xml_document doc;
    doc.load_string(xml_data.c_str());

    if (db_type == DatabaseType::PUBMED) {
        // 处理PubMed格式的文献
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
    } else {
        // 处理PMC格式的文献
        for (pugi::xml_node article : doc.child("pmc-articleset").children("article")) {
            std::string doc_id;
            pugi::xml_node id_node = article.select_node(".//article-id[@pub-id-type='pmc']").node();
            if (id_node) doc_id = id_node.text().as_string();

            InvertedIndex::DocumentMeta meta;

            // 提取标题
            pugi::xml_node title_node = article.select_node(".//article-title").node();
            if (title_node) meta.title = title_node.text().as_string();

            // 提取摘要
            pugi::xml_node abstract_node = article.select_node(".//abstract").node();
            if (abstract_node) {
                for (pugi::xml_node p : abstract_node.children("p")) {
                    meta.abstract += p.text().as_string() + ' ';
                }
            }

            // 提取作者
            pugi::xml_node contrib_group = article.select_node(".//contrib-group").node();
            if (contrib_group) {
                for (pugi::xml_node contrib : contrib_group.children("contrib")) {
                    if (std::string(contrib.attribute("contrib-type").value()) == "author") {
                        pugi::xml_node name = contrib.child("name");
                        if (name) {
                            pugi::xml_node surname = name.child("surname");
                            pugi::xml_node given_names = name.child("given-names");
                            if (surname && given_names) {
                                meta.authors += surname.text().as_string();
                                meta.authors += ", " + std::string(given_names.text().as_string()) + "; ";
                            }
                        }
                    }
                }
            }

            // 提取DOI
            pugi::xml_node doi_node = article.select_node(".//article-id[@pub-id-type='doi']").node();
            if (doi_node) meta.doi = doi_node.text().as_string();

            // 提取出版日期
            pugi::xml_node pub_date_node = article.select_node(".//pub-date[@pub-type='epub']").node();
            if (!pub_date_node) {
                pub_date_node = article.select_node(".//pub-date").node();
            }
            if (pub_date_node) {
                pugi::xml_node year = pub_date_node.child("year");
                if (year) {
                    meta.pub_date = year.text().as_string();
                }
            }

            // 提取关键词
            pugi::xml_node kwd_group = article.select_node(".//kwd-group").node();
            if (kwd_group) {
                for (pugi::xml_node kwd : kwd_group.children("kwd")) {
                    meta.keywords += kwd.text().as_string() + ';';
                }
            }

            // 添加到倒排索引
            if (!doc_id.empty()) {
                index.add_document(doc_id, meta);
                std::cout << "Added to index: " << doc_id << " - " << meta.title << std::endl;
            }
        }
    }

    // 执行检索
    std::cout << "Executing search:" << std::endl;
    std::cout << "Search type: " << (search_type == 1 ? "Keyword search" : 
        (search_type == 2 ? "Boolean search" : "Phrase search")) << std::endl;
    std::cout << "Search query: " << query << std::endl;
    auto search_results = index.enhanced_search(query);

    // 显示检索结果
    std::cout << "Search results:" << std::endl;
    if (search_results.empty()) {
        std::cout << "No relevant documents found." << std::endl;
    } else {
        // 实现分页显示
        const int page_size = 5;  // 每页显示5篇文献
        PaginationResult paginated_results = get_paginated_results(search_results, page_size, 1);
        
        std::cout << "Found " << search_results.size() << " relevant documents (Page " 
                 << paginated_results.current_page << " of " << paginated_results.total_pages << "):" << std::endl;

        // 处理查询词，用于高亮显示
        std::vector<std::string> highlight_terms = tokenize(query);
        for (auto& term : highlight_terms) {
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
        }

        // 显示当前页的文献
        for (const auto& result : paginated_results.results) {
            const auto& doc_id = result.first;
            double score = result.second;
            const auto& meta = index.get_document_meta(doc_id);

            // 高亮显示标题和摘要中的关键词
            std::string highlighted_title = highlight_keywords(meta.title, highlight_terms);
            std::string highlighted_abstract = highlight_keywords(meta.abstract, highlight_terms);

            std::cout << "\nDocument ID: " << doc_id << std::endl
                << "Title: " << highlighted_title << std::endl
                << "Abstract: " << highlighted_abstract.substr(0, 200) << "..." << std::endl
                << "Authors: " << meta.authors << std::endl
                << "Publication Date: " << meta.pub_date << std::endl
                << "Relevance Score: " << score << std::endl
                << "-------------------------------" << std::endl;
        }

        // 分页导航
        std::cout << "\nPage Navigation:" << std::endl;
        std::cout << "Current Page: " << paginated_results.current_page << " of " << paginated_results.total_pages << std::endl;
        if (paginated_results.current_page > 1) {
            std::cout << "Previous page available (Enter 'P')" << std::endl;
        }
        if (paginated_results.current_page < paginated_results.total_pages) {
            std::cout << "Next page available (Enter 'N')" << std::endl;
        }

        // 提供交互式分页控制
        char page_command;
        std::cout << "\nEnter 'P' for previous page, 'N' for next page, or any other key to exit: ";
        std::cin >> page_command;
        
        while (page_command == 'P' || page_command == 'N' || page_command == 'p' || page_command == 'n') {
            if ((page_command == 'P' || page_command == 'p') && paginated_results.current_page > 1) {
                paginated_results = get_paginated_results(search_results, page_size, paginated_results.current_page - 1);
            }
            else if ((page_command == 'N' || page_command == 'n') && paginated_results.current_page < paginated_results.total_pages) {
                paginated_results = get_paginated_results(search_results, page_size, paginated_results.current_page + 1);
            }

            // 显示新页面的结果
            for (const auto& result : paginated_results.results) {
                const auto& doc_id = result.first;
                double score = result.second;
                const auto& meta = index.get_document_meta(doc_id);

                std::string highlighted_title = highlight_keywords(meta.title, highlight_terms);
                std::string highlighted_abstract = highlight_keywords(meta.abstract, highlight_terms);

                std::cout << "\nDocument ID: " << doc_id << std::endl
                    << "Title: " << highlighted_title << std::endl
                    << "Abstract: " << highlighted_abstract.substr(0, 200) << "..." << std::endl
                    << "Authors: " << meta.authors << std::endl
                    << "Publication Date: " << meta.pub_date << std::endl
                    << "Relevance Score: " << score << std::endl
                    << "-------------------------------" << std::endl;
            }

            std::cout << "\nPage Navigation:" << std::endl;
            std::cout << "Current Page: " << paginated_results.current_page << " of " << paginated_results.total_pages << std::endl;
            if (paginated_results.current_page > 1) {
                std::cout << "Previous page available (Enter 'P')" << std::endl;
            }
            if (paginated_results.current_page < paginated_results.total_pages) {
                std::cout << "Next page available (Enter 'N')" << std::endl;
            }
            
            std::cout << "\nEnter 'P' for previous page, 'N' for next page, or any other key to exit: ";
            std::cin >> page_command;
        }
    }

    // 趋势分析部分
    std::cout << "\nTrend Analysis:" << std::endl;
    std::cout << "Publication trend for '" << query << "' over years:" << std::endl;
    auto trend = index.analyze_trend(query);
    
    // 找出最大计数以便绘制ASCII图表
    int max_count = 0;
    for (const auto& pair : trend.yearly_counts) {
        max_count = max(max_count, pair.second);
    }

    // 绘制ASCII趋势图
    for (const auto& pair : trend.yearly_counts) {
        std::cout << pair.first << " | ";
        int bar_length = (pair.second * 50) / max_count;  // 标准化到50个字符的宽度
        std::cout << std::string(bar_length, '*') << " (" << pair.second << " publications)" << std::endl;
    }

    // 词云分析
    std::cout << "\nWord Cloud Analysis:" << std::endl;
    if (!search_results.empty()) {
        const auto& first_doc_id = search_results[0].first;
        auto wordcloud_data = index.generate_wordcloud_data(first_doc_id);
        std::cout << "Most frequent terms in the top document:" << std::endl;
        for (size_t i = 0; i < min(size_t(10), wordcloud_data.size()); ++i) {
            std::cout << std::string(wordcloud_data[i].frequency, '*') << " " 
                     << wordcloud_data[i].word << " (" << wordcloud_data[i].frequency << " occurrences)" << std::endl;
        }
    }

    return 0;
}

