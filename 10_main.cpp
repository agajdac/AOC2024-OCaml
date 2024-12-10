#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <set>

struct Node
{
    std::multiset<std::pair<int, int>> set{}; // std::set to solve part 1
    int val = 0;
    bool visited = false;
};

std::vector<std::vector<Node>> matrix;

bool shouldVisit(int i, int j, int prev)
{
    if (i < 0 || j < 0 || j >= matrix.size() || i >= matrix.size())
    {
        return false;
    }

    if (matrix[i][j].val - 1 == prev)
    {
        return true;
    }
    
    return false;
}

void visit(int i, int j)
{
    if (i < 0 || j < 0 || j >= matrix.size() || i >= matrix.size())
    {
        return;
    }
    if (matrix[i][j].visited)
    {
        return;
    }

    matrix[i][j].visited = true;
    if(matrix[i][j].val == 9)
    {
        matrix[i][j].set.insert({i, j});
        return;
    }

    if (shouldVisit(i + 1, j, matrix[i][j].val))
    {
        visit(i + 1, j);
        matrix[i][j].set.insert(matrix[i + 1][j].set.begin(), matrix[i + 1][j].set.end());
    }
    if (shouldVisit(i - 1, j, matrix[i][j].val))
    {
        visit(i - 1, j);
        matrix[i][j].set.insert(matrix[i - 1][j].set.begin(), matrix[i - 1][j].set.end());
    }
    if (shouldVisit(i, j + 1, matrix[i][j].val))
    {
        visit(i, j + 1);
        matrix[i][j].set.insert(matrix[i][j + 1].set.begin(), matrix[i][j + 1].set.end());
    }
    if (shouldVisit(i, j - 1, matrix[i][j].val))
    {
        visit(i, j - 1);
        matrix[i][j].set.insert(matrix[i][j - 1].set.begin(), matrix[i][j - 1].set.end());
    }
}

int solve()
{
    matrix.clear();
    std::ifstream file("data.txt");

    std::string line;
    while (std::getline(file, line)) 
    {
        std::string ss(line);
        std::vector<Node> row;
        int number;
        for(char c : ss)
        {
            Node node;
            node.val = c - '0';
            row.push_back(node);
        }
        matrix.push_back(row);
    }

    int ans = 0;
    for(int i = 0; i < matrix.size(); ++i)
    {
        for(int j = 0; j < matrix.size(); ++j)
        {
            visit(i, j);
            if(matrix[i][j].val == 0)
            {
                ans += matrix[i][j].set.size();   
            }
        }
    }
    return ans;
}

int main() 
{
    std::cout << solve() << '\n';
    return 0;
}
