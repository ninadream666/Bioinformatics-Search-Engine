#ifndef DATA_H
#define DATA_H

#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <memory>
#include <algorithm>

class InvertedIndex {
public:
    // 文档元数据结构
    struct DocumentMeta {
        std::string title;
        std::string abstract;
        std::string authors;
        std::string doi;
        std::string pub_date;
        std::string keywords;
    };

    // 趋势分析数据结构
    struct TrendData {
        std::string term;
        std::vector<std::pair<std::string, int>> yearly_counts;
    };

    // 词云图数据结构
    struct WordCloudData {
        std::string word;
        int frequency;
    };

    // 分页结果结构体
    struct SearchResult {
        std::vector<std::string> doc_ids;
        int total_pages;
        int current_page;
    };

    // 公共成员函数
    void add_document(const std::string& doc_id, const DocumentMeta& meta);
    std::vector<std::pair<std::string, double>> search(const std::string& query);
    std::vector<std::pair<std::string, double>> enhanced_search(const std::string& query);
    DocumentMeta get_document_meta(const std::string& doc_id) const;
    std::vector<std::string> rank_by_bm25(const std::string& term);
    std::vector<std::string> phrase_search(const std::vector<std::string>& phrase);
    SearchResult get_paginated_results(const std::vector<std::string>& doc_ids, int page_size, int page_num);
    std::string highlight_keywords(const std::string& text, const std::vector<std::string>& keywords);
    TrendData analyze_trend(const std::string& term) const;
    std::vector<WordCloudData> generate_wordcloud_data(const std::string& doc_id);
    void print_index();
    std::vector<std::string> execute_boolean_search(const struct BooleanNode& node);
    double calculate_bm25_score(const std::string& doc_id, const std::string& term);

private:
    std::unordered_map<std::string, std::unordered_set<std::string>> index;
    std::unordered_map<std::string, std::vector<std::string>> doc_terms;
    std::unordered_map<std::string, DocumentMeta> doc_meta;
};

// 布尔检索相关结构
enum class BooleanOperator {
    AND,
    OR,
    NOT
};

struct BooleanNode {
    std::string term;
    BooleanOperator op;
    std::vector<BooleanNode> children;
};

// 函数声明
BooleanNode parse_boolean_expression(const std::string& query);
std::vector<std::string> tokenize(const std::string& text);
std::string stem_word(const std::string& word);
bool is_stopword(const std::string& word);
std::vector<std::string> remove_stopwords(const std::vector<std::string>& tokens);

#endif // DATA_H