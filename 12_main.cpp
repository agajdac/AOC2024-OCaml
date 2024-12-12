#include <fstream>
#include <iostream>
#include <vector>

struct Node
{
    char c{};
    int areas = 0;
    int perimeter = 0;
    int corners = 0;
    bool visited = false;
};
std::vector<std::vector<Node>> matrix;

bool is_valid(int x, int y, char c)
{
    if (x < 0 or y < 0 or x >= matrix.size() or y >= matrix.size())
    {
        return false;
    }
    if (c != matrix[x][y].c)
    {
        return false;
    }

    return true;
}

void search(int x, int y)
{
    if (matrix[x][y].visited)
    {
        return;
    }
    matrix[x][y].areas = 1;
    matrix[x][y].visited = true;

    const auto A = std::make_pair<int, int>(x - 1, y + 0);// Up
    const auto B = std::make_pair<int, int>(x + 1, y + 0);// Down
    const auto C = std::make_pair<int, int>(x + 0, y + 1);// Right
    const auto D = std::make_pair<int, int>(x + 0, y - 1);// Left
    const auto E = std::make_pair<int, int>(x - 1, y - 1);// Up Left
    const auto F = std::make_pair<int, int>(x + 1, y + 1);// Down Right
    const auto G = std::make_pair<int, int>(x - 1, y + 1);// Right Up
    const auto H = std::make_pair<int, int>(x + 1, y - 1);// Left Down

    if (not is_valid(A.first, A.second, matrix[x][y].c) && not is_valid(C.first, C.second, matrix[x][y].c))
    {
        ++matrix[x][y].corners;
    }
    if (not is_valid(A.first, A.second, matrix[x][y].c) && not is_valid(D.first, D.second, matrix[x][y].c))
    {
        ++matrix[x][y].corners;
    }
    if (not is_valid(B.first, B.second, matrix[x][y].c) && not is_valid(C.first, C.second, matrix[x][y].c))
    {
        ++matrix[x][y].corners;
    }
    if (not is_valid(B.first, B.second, matrix[x][y].c) && not is_valid(D.first, D.second, matrix[x][y].c))
    {
        ++matrix[x][y].corners;
    }


    if (is_valid(A.first, A.second, matrix[x][y].c) and is_valid(C.first, C.second, matrix[x][y].c)
        and not is_valid(G.first, G.second, matrix[x][y].c))
    {
        ++matrix[x][y].corners;
    }
    if (is_valid(B.first, B.second, matrix[x][y].c) and is_valid(C.first, C.second, matrix[x][y].c)
        and not is_valid(F.first, F.second, matrix[x][y].c))
    {
        ++matrix[x][y].corners;
    }
    if (is_valid(B.first, B.second, matrix[x][y].c) and is_valid(D.first, D.second, matrix[x][y].c)
        and not is_valid(H.first, H.second, matrix[x][y].c))
    {
        ++matrix[x][y].corners;
    }
    if (is_valid(A.first, A.second, matrix[x][y].c) and is_valid(D.first, D.second, matrix[x][y].c)
        and not is_valid(E.first, E.second, matrix[x][y].c))
    {
        ++matrix[x][y].corners;
    }
    std::vector<std::pair<int, int>> vec{ { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } };
    for (const auto& [ax, ay] : vec)
    {
        const auto [ox, oy] = std::make_pair<int, int>(x - ax, y - ay);
        if (is_valid(ox, oy, matrix[x][y].c))
        {
            if (not matrix[ox][oy].visited)
            {
                search(ox, oy);
                matrix[x][y].areas += matrix[ox][oy].areas;
                matrix[x][y].perimeter += matrix[ox][oy].perimeter;
                matrix[x][y].corners += matrix[ox][oy].corners;
            }
        }
        else
        {
            ++matrix[x][y].perimeter;
        }
    }
}


int main()
{
    matrix.clear();
    std::ifstream file("data.txt");

    std::string line;
    while (std::getline(file, line))
    {
        std::string ss(line);
        std::vector<Node> row;
        int number;
        for (char c : ss)
        {
            Node node;
            node.c = c;
            row.push_back(node);
        }
        matrix.push_back(row);
    }

    unsigned sum = 0;
    unsigned sum_2 = 0;
    for (int i = 0; i < matrix.size(); ++i)
    {
        for (int j = 0; j < matrix.size(); ++j)
        {
            if (not matrix[i][j].visited)
            {
                search(i, j);
                sum += matrix[i][j].corners * matrix[i][j].areas;
                sum_2 += matrix[i][j].perimeter * matrix[i][j].areas;
            }
        }
    }

    std::cout << "Part 1: " << sum_2 << '\n';
    std::cout << "Part 2: " << sum << '\n';
}
