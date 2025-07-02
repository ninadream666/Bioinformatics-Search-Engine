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

// �ص����������� libcurl ����
size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

// ��ȡ HTTP ����
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

// �����������ݴ������ݿ�
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
        std::cout << "�����Ѵ������ݿ⣺" << title << std::endl;

        delete con;
    }
    catch (sql::SQLException& e) {
        std::cerr << "���ݿ�洢ʧ�ܣ�" << e.what() << std::endl;
    }
}

// ���� PubMed XML ���������ݿ�
void parse_xml(const std::string& xml_data) {
    pugi::xml_document doc;
    pugi::xml_parse_result result = doc.load_string(xml_data.c_str());

    if (!result) {
        std::cerr << "XML Parsing error: " << result.description() << std::endl;
        return;
    }

    for (pugi::xml_node entry : doc.child("PubmedArticleSet").children("PubmedArticle")) {
        std::string title, abstract_text, authors, doi, pub_date, keywords;

        // ��ȡ����
        pugi::xml_node title_node = entry.child("MedlineCitation").child("Article").child("ArticleTitle");
        if (title_node) title = title_node.text().as_string();

        // ��ȡժҪ
        pugi::xml_node abstract_node = entry.child("MedlineCitation").child("Article").child("Abstract");
        if (abstract_node) {
            for (pugi::xml_node abstract_text_node : abstract_node.children("AbstractText")) {
                abstract_text += abstract_text_node.text().as_string() + ' ';
            }
        }

        // ��ȡ����
        for (pugi::xml_node author : entry.child("MedlineCitation").child("Article").child("AuthorList").children("Author")) {
            pugi::xml_node last_name = author.child("LastName");
            pugi::xml_node first_name = author.child("ForeName");
            if (last_name && first_name) {
                authors += last_name.text().as_string();
                authors += ", " + std::string(first_name.text().as_string()) + "; ";
            }
        }

        // ��ȡ DOI
        pugi::xml_node doi_node = entry.child("PubmedData").child("ArticleIdList").find_child_by_attribute("ArticleId", "IdType", "doi");
        if (doi_node) doi = doi_node.text().as_string();

        // ��ȡ��������
        pugi::xml_node pub_date_node = entry.child("MedlineCitation").child("Article").child("Journal").child("JournalIssue").child("PubDate");
        if (pub_date_node) pub_date = pub_date_node.child("Year").text().as_string();

        // ��ȡ�ؼ���
        for (pugi::xml_node mesh_heading : entry.child("MedlineCitation").child("MeshHeadingList").children("MeshHeading")) {
            pugi::xml_node descriptor = mesh_heading.child("DescriptorName");
            if (descriptor) {
                keywords += descriptor.text().as_string();
                keywords += "; ";
            }
        }

        // �������ݿ�
        save_to_database(title, abstract_text, authors, doi, pub_date, keywords);

        // ���������Ϣ
        std::cout << "����: " << title << std::endl;
        std::cout << "ժҪ: " << abstract_text << std::endl;
        std::cout << "����: " << authors << std::endl;
        std::cout << "DOI: " << doi << std::endl;
        std::cout << "��������: " << pub_date << std::endl;
        std::cout << "�ؼ���: " << keywords << std::endl;
        std::cout << "-------------------------------" << std::endl;
    }
}

// �ı�Ԥ�������ִ�
std::vector<std::string> tokenize(const std::string& text) {
    std::vector<std::string> tokens;
    boost::tokenizer<> tok(text);
    for (auto& word : tok) {
        tokens.push_back(word);
    }
    return tokens;
}

// �ı�Ԥ�������ʸ���ȡ
std::string stem_word(const std::string& word) {
    struct sb_stemmer* stemmer = sb_stemmer_new("english", NULL);
    if (!stemmer) return word;

    const sb_symbol* stemmed = sb_stemmer_stem(stemmer, (const sb_symbol*)word.c_str(), word.size());
    std::string result((const char*)stemmed);

    sb_stemmer_delete(stemmer);
    return result;
}

// �ı�Ԥ������ȥ��ͣ�ô�
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

// ��������������
enum class BooleanOperator {
    AND,
    OR,
    NOT
};

// �������ʽ�ڵ�
struct BooleanNode {
    std::string term;
    BooleanOperator op;
    std::vector<BooleanNode> children;
};

// �����������ʽ��ǰ������
BooleanNode parse_boolean_expression(const std::string& query);

// ����������
class InvertedIndex {
public:
    // ����ĵ�Ԫ���ݽṹ
    struct DocumentMeta {
        std::string title;
        std::string abstract;
        std::string authors;
        std::string doi;
        std::string pub_date;
        std::string keywords;
    };

    // �޸�����ĵ��ķ���
    void add_document(const std::string& doc_id, const DocumentMeta& meta) {
        std::cout << "���ڴ����ĵ�: " << doc_id << std::endl;

        // �������
        std::vector<std::string> title_terms = tokenize(meta.title);
        title_terms = remove_stopwords(title_terms);
        for (auto& term : title_terms) {
            // ת��ΪСд
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
            if (!term.empty()) {
                index[term].insert(doc_id);
                doc_terms[doc_id].push_back(term);
                std::cout << "���������: " << term << " -> " << doc_id << std::endl;
            }
        }

        // ����ժҪ
        std::vector<std::string> abstract_terms = tokenize(meta.abstract);
        abstract_terms = remove_stopwords(abstract_terms);
        for (auto& term : abstract_terms) {
            // ת��ΪСд
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
            if (!term.empty()) {
                index[term].insert(doc_id);
                doc_terms[doc_id].push_back(term);
                std::cout << "����ժҪ��: " << term << " -> " << doc_id << std::endl;
            }
        }

        // ����ؼ���
        std::vector<std::string> keyword_terms = tokenize(meta.keywords);
        keyword_terms = remove_stopwords(keyword_terms);
        for (auto& term : keyword_terms) {
            // ת��ΪСд
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
            if (!term.empty()) {
                index[term].insert(doc_id);
                doc_terms[doc_id].push_back(term);
                std::cout << "�����ؼ���: " << term << " -> " << doc_id << std::endl;
            }
        }

        // �洢�ĵ�Ԫ����
        doc_meta[doc_id] = meta;
        std::cout << "�ĵ� " << doc_id << " ������ɣ������� " << doc_terms[doc_id].size() << " ����" << std::endl;
    }

    // �޸�����������ʹ��enhanced_search
    std::vector<std::pair<std::string, double>> search(const std::string& query) {
        return enhanced_search(query);
    }

    // ��ȡ�ĵ�Ԫ����
    DocumentMeta get_document_meta(const std::string& doc_id) const {
        auto it = doc_meta.find(doc_id);
        if (it != doc_meta.end()) {
            return it->second;
        }
        return DocumentMeta();
    }

    // ���BM25������
    std::vector<std::string> rank_by_bm25(const std::string& term) {
        std::vector<std::pair<std::string, double>> results;
        const double k1 = 1.5;
        const double b = 0.75;

        // ����ƽ���ĵ�����
        double avg_doc_length = 0;
        for (const auto& doc : doc_terms) {
            avg_doc_length += doc.second.size();
        }
        avg_doc_length /= doc_terms.size();

        // ����IDF
        int doc_freq = index[term].size();
        double idf = (doc_freq > 0) ? log((double)doc_terms.size() / doc_freq) : 0;

        // ����ÿ���ĵ���BM25����
        for (const auto& doc : doc_terms) {
            const auto& doc_id = doc.first;
            const auto& terms = doc.second;

            // ����TF
            int term_count = std::count(terms.begin(), terms.end(), term);
            double tf = (double)term_count / terms.size();

            // �����ĵ����ȹ�һ������
            double doc_length = terms.size();
            double length_norm = (1 - b) + b * (doc_length / avg_doc_length);

            // ����BM25����
            double bm25_score = (idf * tf * (k1 + 1)) / (tf + k1 * length_norm);
            results.emplace_back(doc_id, bm25_score);
        }

        // ��������������
        std::sort(results.begin(), results.end(), [](const auto& a, const auto& b) {
            return a.second > b.second;
            });

        // ֻ�����ĵ�ID
        std::vector<std::string> doc_ids;
        for (const auto& result : results) {
            doc_ids.push_back(result.first);
        }

        return doc_ids;
    }

    // �������
    std::vector<std::string> phrase_search(const std::vector<std::string>& phrase) {
        std::vector<std::string> results;
        if (phrase.empty()) return results;

        // ��ȡ��һ���ʵ������ĵ�
        std::unordered_set<std::string> candidate_docs = index[phrase[0]];

        for (const auto& doc_id : candidate_docs) {
            const auto& terms = doc_terms[doc_id];
            bool found = false;

            // ���ĵ��в��Ҷ���
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

    // ��ҳ���
    struct SearchResult {
        std::vector<std::string> doc_ids;
        int total_pages;
        int current_page;
    };

    SearchResult get_paginated_results(const std::vector<std::string>& doc_ids, int page_size, int page_num) {
        SearchResult result;
        int total_docs = doc_ids.size();
        int total_pages = (total_docs + page_size - 1) / page_size;

        // ȷ��ҳ����Ч
        if (page_num < 1) page_num = 1;
        if (page_num > total_pages) page_num = total_pages;

        // ���㵱ǰҳ���ĵ�
        int start_idx = (page_num - 1) * page_size;
        int end_idx = (start_idx + page_size > total_docs) ? total_docs : start_idx + page_size;

        result.doc_ids.assign(doc_ids.begin() + start_idx, doc_ids.begin() + end_idx);
        result.total_pages = total_pages;
        result.current_page = page_num;

        return result;
    }

    // �ؼ��ʸ���
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

    // ���Ʒ���
    struct TrendData {
        std::string term;
        std::vector<std::pair<std::string, int>> yearly_counts;
    };

    TrendData analyze_trend(const std::string& term, const std::unordered_map<std::string, std::string>& doc_dates) {
        TrendData trend;
        trend.term = term;

        // ͳ��ÿ��ĳ��ִ���
        std::unordered_map<std::string, int> year_counts;
        for (const auto& doc : index[term]) {
            auto it = doc_dates.find(doc);
            if (it != doc_dates.end()) {
                std::string year = it->second.substr(0, 4);
                year_counts[year]++;
            }
        }

        // ת��Ϊ������vector
        for (const auto& pair : year_counts) {
            trend.yearly_counts.push_back(pair);
        }

        std::sort(trend.yearly_counts.begin(), trend.yearly_counts.end(),
            [](const auto& a, const auto& b) { return a.first < b.first; });

        return trend;
    }

    // ����ͼ��������
    struct WordCloudData {
        std::string word;
        int frequency;
    };

    std::vector<WordCloudData> generate_wordcloud_data(const std::string& doc_id) {
        std::vector<WordCloudData> wordcloud;
        std::unordered_map<std::string, int> word_freq;

        // ͳ�ƴ�Ƶ
        const auto& terms = doc_terms[doc_id];
        for (const auto& term : terms) {
            word_freq[term]++;
        }

        // ת��ΪWordCloudData��ʽ
        for (const auto& pair : word_freq) {
            wordcloud.push_back({ pair.first, pair.second });
        }

        // ��Ƶ������
        std::sort(wordcloud.begin(), wordcloud.end(),
            [](const auto& a, const auto& b) { return a.frequency > b.frequency; });

        return wordcloud;
    }

    void print_index() {
        std::cout << "�����������ݣ�" << std::endl;
        for (const auto& pair : index) {
            std::cout << "����: " << pair.first << " -> �ĵ�ID: ";
            for (const auto& doc_id : pair.second) {
                std::cout << doc_id << " ";
            }
            std::cout << std::endl; // ����
        }
    }

    // ִ�в�������
    std::vector<std::string> execute_boolean_search(const BooleanNode& node) {
        std::vector<std::string> results;

        if (node.children.empty()) {
            // Ҷ�ӽڵ㣬ִ�йؼ�������
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

    // �޸�BM25�������㺯��
    double calculate_bm25_score(const std::string& doc_id, const std::string& term) {
        const double k1 = 1.5;
        const double b = 0.75;

        // ����ƽ���ĵ�����
        double avg_doc_length = 0;
        for (const auto& doc : doc_terms) {
            avg_doc_length += doc.second.size();
        }
        if (doc_terms.empty()) return 0.0;
        avg_doc_length /= doc_terms.size();

        // ����IDF
        int doc_freq = index[term].size();
        if (doc_freq == 0) return 0.0;
        // �޸�IDF���㣬ʹ�ø���׼�Ĺ�ʽ
        double idf = log((double)(doc_terms.size() - doc_freq + 0.5) / (doc_freq + 0.5) + 1.0);

        // ����TF
        const auto& terms = doc_terms[doc_id];
        if (terms.empty()) return 0.0;
        int term_count = std::count(terms.begin(), terms.end(), term);
        if (term_count == 0) return 0.0;

        // ����TF�����Ǵʵ�λ��Ȩ��
        double tf = 0.0;
        for (size_t i = 0; i < terms.size(); ++i) {
            if (terms[i] == term) {
                // �����еĴ�Ȩ�ظ���
                if (i < 10) {  // ����ǰ10�����Ǳ���
                    tf += 2.0;
                }
                else {
                    tf += 1.0;
                }
            }
        }
        tf /= terms.size();

        // �����ĵ����ȹ�һ������
        double doc_length = terms.size();
        double length_norm = (1 - b) + b * (doc_length / avg_doc_length);

        // ����BM25����
        double score = (idf * tf * (k1 + 1)) / (tf + k1 * length_norm);

        // ��ӵ�����Ϣ
        std::cout << "�ĵ� " << doc_id << " �Ĵ� " << term << " ��BM25���㣺" << std::endl;
        std::cout << "  - ��Ƶ(TF): " << tf << std::endl;
        std::cout << "  - ���ĵ�Ƶ��(IDF): " << idf << std::endl;
        std::cout << "  - �ĵ����ȹ�һ��: " << length_norm << std::endl;
        std::cout << "  - ���յ÷�: " << score << std::endl;

        return score;
    }

    // �޸�enhanced_search����
    std::vector<std::pair<std::string, double>> enhanced_search(const std::string& query) {
        // Ԥ�����ѯ��
        std::vector<std::string> query_terms = tokenize(query);
        query_terms = remove_stopwords(query_terms);
        for (auto& term : query_terms) {
            // ת��ΪСд
            std::transform(term.begin(), term.end(), term.begin(), ::tolower);
            term = stem_word(term);
        }

        std::cout << "�����Ĳ�ѯ��: ";
        for (const auto& term : query_terms) {
            std::cout << term << " ";
        }
        std::cout << std::endl;

        if (query_terms.empty()) {
            return std::vector<std::pair<std::string, double>>();
        }

        // ����Ƿ��ǲ�����ѯ
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

        // ����Ƿ��Ƕ����ѯ
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

        // ��ͨ�ؼ��ʲ�ѯ
        std::unordered_set<std::string> candidate_docs;
        for (const auto& term : query_terms) {
            auto it = index.find(term);
            if (it != index.end()) {
                std::cout << "�ҵ��� " << term << " ���ĵ�: ";
                for (const auto& doc_id : it->second) {
                    std::cout << doc_id << " ";
                }
                std::cout << std::endl;
                candidate_docs.insert(it->second.begin(), it->second.end());
            }
            else {
                std::cout << "δ�ҵ��� " << term << " ���ĵ�" << std::endl;
            }
        }

        std::vector<std::pair<std::string, double>> ranked_results;
        for (const auto& doc_id : candidate_docs) {
            double score = 0.0;
            for (const auto& term : query_terms) {
                double term_score = calculate_bm25_score(doc_id, term);
                score += term_score;
                std::cout << "�ĵ� " << doc_id << " �Ĵ� " << term << " �÷�: " << term_score << std::endl;
            }
            if (score > 0) {
                ranked_results.push_back({ doc_id, score });
                std::cout << "�ĵ� " << doc_id << " ���ܵ÷�: " << score << std::endl;
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

// �����������ʽ
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

// ������
int main() {
    std::string query;  // �û�����Ĳ�ѯ�ؼ���
    int max_results = 20;  // ���ص������������Ĭ�� 20
    int search_type;  // ������ʽѡ��

    // ��ʾ�û�ѡ�������ʽ
    std::cout << "��ѡ�������ʽ��" << std::endl;
    std::cout << "1. �ؼ��ʼ�����ֱ������ؼ��ʣ�" << std::endl;
    std::cout << "2. ����������ʹ�� AND��OR��NOT���磺cancer AND treatment��" << std::endl;
    std::cout << "3. ���������ʹ��˫���ţ��磺\"cancer treatment\"��" << std::endl;
    std::cout << "�����������ʽ��1-3����";
    std::cin >> search_type;
    std::cin.ignore();  // ���Իس���

    // ��֤������ʽѡ��
    if (search_type < 1 || search_type > 3) {
        std::cerr << "������Ч�ļ�����ʽѡ��" << std::endl;
        return 1;
    }

    // ���ݼ�����ʽ��ʾ�û�����
    switch (search_type) {
        case 1:
            std::cout << "������ؼ��ʣ��磺cancer����";
            break;
        case 2:
            std::cout << "�����벼�����ʽ���磺cancer AND treatment����";
            break;
        case 3:
            std::cout << "����������˫���Ű�Χ���磺\"cancer treatment\"����";
            break;
    }
    std::getline(std::cin, query);

    // ��ʾ�û����뷵�ص����������
    std::cout << "�����뷵�ص������������Ĭ�� 20������ 0 �����ƣ���";
    std::cin >> max_results;  // ��ȡ�û���������������
    std::cin.ignore();  // ���Իس�����ȷ��������������

    // �����ѯ�ؼ���Ϊ�գ����������Ϣ���˳�
    if (query.empty()) {
        std::cerr << "���󣺲�ѯ�ؼ��ʲ���Ϊ�գ�" << std::endl;
        return 1;
    }

    // ��֤���������ʽ
    switch (search_type) {
        case 2:  // ��������
            if (query.find(" AND ") == std::string::npos &&
                query.find(" OR ") == std::string::npos &&
                query.find(" NOT ") == std::string::npos) {
                std::cerr << "���󣺲�������������� AND��OR �� NOT ��������" << std::endl;
                return 1;
            }
            break;
        case 3:  // �������
            if (query.front() != '"' || query.back() != '"') {
                std::cerr << "���󣺶������������˫���Ű�Χ��" << std::endl;
                return 1;
            }
            break;
    }

    // ʹ�� CURL �Բ�ѯ�ؼ��ʽ��� URL ����
    CURL* curl = curl_easy_init();  // ��ʼ�� CURL ����
    char* encoded_query = curl_easy_escape(curl, query.c_str(), query.length());  // URL ����
    std::string query_param = encoded_query ? encoded_query : query;  // �������ɹ���ʹ�ñ����Ĳ�ѯ�ʣ�����ʹ��ԭ��ѯ��
    curl_free(encoded_query);  // �ͷű������ַ���
    curl_easy_cleanup(curl);  // ���� CURL ����

    // ���� PubMed ��ѯ�� URL
    std::string esearch_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=" + query_param + "&retmode=xml";
    if (max_results > 0) {
        esearch_url += "&retmax=" + std::to_string(max_results);  // ����û������������������� 0����� retmax ����
    }

    // ��ȡ PubMed ��ѯ����� XML ����
    std::string xml_search_result = fetch_data_from_url(esearch_url);  // ��ָ�� URL ��ȡ����
    pugi::xml_document search_doc;  // ���� XML �ĵ�����
    search_doc.load_string(xml_search_result.c_str());  // ����ȡ�� XML ���ݼ��ص��ĵ���

    // ��ȡ���� ID �б�
    std::string id_list;
    for (pugi::xml_node id_node : search_doc.child("eSearchResult").child("IdList").children("Id")) {
        id_list += id_node.text().as_string();  // ��ȡÿ�����׵� ID
        id_list += ",";  // �ö��ŷָ� ID
    }
    if (!id_list.empty()) id_list.pop_back();  // �Ƴ����һ������

    // ���û���ҵ�������ף������ʾ���˳�
    if (id_list.empty()) {
        std::cerr << "δ�ҵ�������ף��볢�������ؼ��ʡ�" << std::endl;
        return 1;
    }

    // ���� PubMed ��ȡ������ϸ��Ϣ�� URL
    std::string efetch_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=" + id_list + "&retmode=xml";
    std::string xml_data = fetch_data_from_url(efetch_url);  // ��ȡ��ϸ��������

    // ���� XML ���ݲ����������Ϣ
    parse_xml(xml_data);

    // ������������
    InvertedIndex index;
    pugi::xml_document doc;
    doc.load_string(xml_data.c_str());  // ʹ�û�ȡ������ϸ��������

    for (pugi::xml_node entry : doc.child("PubmedArticleSet").children("PubmedArticle")) {
        std::string doc_id;
        pugi::xml_node id_node = entry.child("PubmedData").child("ArticleIdList").child("ArticleId");
        if (id_node) doc_id = id_node.text().as_string();

        InvertedIndex::DocumentMeta meta;

        // ��ȡ����
        pugi::xml_node title_node = entry.child("MedlineCitation").child("Article").child("ArticleTitle");
        if (title_node) meta.title = title_node.text().as_string();

        // ��ȡժҪ
        pugi::xml_node abstract_node = entry.child("MedlineCitation").child("Article").child("Abstract");
        if (abstract_node) {
            for (pugi::xml_node abstract_text_node : abstract_node.children("AbstractText")) {
                meta.abstract += abstract_text_node.text().as_string() + ' ';
            }
        }

        // ��ȡ����
        for (pugi::xml_node author : entry.child("MedlineCitation").child("Article").child("AuthorList").children("Author")) {
            pugi::xml_node last_name = author.child("LastName");
            pugi::xml_node first_name = author.child("ForeName");
            if (last_name && first_name) {
                meta.authors += last_name.text().as_string();
                meta.authors += ", " + std::string(first_name.text().as_string()) + "; ";
            }
        }

        // ��ȡDOI
        pugi::xml_node doi_node = entry.child("PubmedData").child("ArticleIdList").find_child_by_attribute("ArticleId", "IdType", "doi");
        if (doi_node) meta.doi = doi_node.text().as_string();

        // ��ȡ��������
        pugi::xml_node pub_date_node = entry.child("MedlineCitation").child("Article").child("Journal").child("JournalIssue").child("PubDate");
        if (pub_date_node) meta.pub_date = pub_date_node.child("Year").text().as_string();

        // ��ȡ�ؼ���
        for (pugi::xml_node mesh_heading : entry.child("MedlineCitation").child("MeshHeadingList").children("MeshHeading")) {
            pugi::xml_node descriptor = mesh_heading.child("DescriptorName");
            if (descriptor) {
                meta.keywords += descriptor.text().as_string();
                meta.keywords += "; ";
            }
        }

        // ��ӵ���������
        index.add_document(doc_id, meta);
    }

    // ִ�м���
    std::cout << "\nִ�м�����" << std::endl;
    std::cout << "������ʽ��" << (search_type == 1 ? "�ؼ��ʼ���" :
        search_type == 2 ? "��������" : "�������") << std::endl;
    std::cout << "�����ʣ�" << query << std::endl;
    auto search_results = index.enhanced_search(query);

    std::cout << "\n���������" << std::endl;
    if (search_results.empty()) {
        std::cout << "δ�ҵ�������ס�" << std::endl;
    }
    else {
        std::cout << "�ҵ� " << search_results.size() << " ƪ������ף�" << std::endl;
        for (const auto& result : search_results) {
            const auto& doc_id = result.first;
            double score = result.second;
            const auto& meta = index.get_document_meta(doc_id);

            std::cout << "\n����ID: " << doc_id << std::endl;
            std::cout << "����: " << meta.title << std::endl;
            std::cout << "����: " << meta.authors << std::endl;
            std::cout << "��������: " << meta.pub_date << std::endl;
            std::cout << "��ضȵ÷�: " << score << std::endl;
            std::cout << "-------------------------------" << std::endl;
        }
    }

    return 0;  // �������
}
