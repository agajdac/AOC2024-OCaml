#include <array>
#include <cmath>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <vector>
#include <thread>  
#include <chrono>  
struct Pos
{
    int x;
    int y;
};

struct Robot
{
    Pos curr;
    Pos vel;
};
std::vector<Robot> vec;


constexpr size_t width = 101;
constexpr size_t height = 103;
constexpr size_t middleX = width / 2;
constexpr size_t middleY = height / 2;
std::array<std::array<int, width>, height> matrix;

std::pair<std::string, std::string> split(const std::string& tekst, char znak)
{
    size_t pozycja = tekst.find(znak);

    if (pozycja == std::string::npos)
    {
        return { tekst, "" };
    }

    std::string pierwszaCzesc = tekst.substr(0, pozycja);
    std::string drugaCzesc = tekst.substr(pozycja + 1);

    return { pierwszaCzesc, drugaCzesc };
}

void print()
{
    for (int i = 0; i < height; ++i)
    {
        for (int j = 0; j < width; ++j)
        {
            matrix[i][j] = 0;
        }
    }
    for (const Robot& robot : vec)
    {
        ++matrix[robot.curr.y][robot.curr.x];
    }
    for (int i = 0; i < height; ++i)
    {
        for (int j = 0; j < width; ++j)
        {
            if (matrix[i][j] == 0)
            {
                std::cout << " ";
            }
            else
            {
                std::cout << "X";
            }
        }
        std::cout << '\n';
    }
    std::cout << '\n';
    std::cout << '\n';
    std::cout << '\n';
}

void move(int n)
{
    for (int i = 0; i < n; ++i)
    {
        for (Robot& robot : vec)
        {
            robot.curr.x += robot.vel.x;
            robot.curr.y += robot.vel.y;
            if (robot.curr.x < 0)
            {
                robot.curr.x = width + robot.curr.x;
            }
            else if (robot.curr.x >= width)
            {
                robot.curr.x -= width;
            }

            if (robot.curr.y < 0)
            {
                robot.curr.y = height + robot.curr.y;
            }
            else if (robot.curr.y >= height)
            {
                robot.curr.y -= height;
            }
        }
        print();
        std::cout << i << '\n';
    }
}

int main()
{

    std::ifstream file("data.txt");
    while (file.good())
    {
        std::string str;
        file >> str;
        str = str.substr(2);
        const auto [x, y] = split(str, ',');
        Pos curr;
        curr.x = std::atoi(x.c_str());
        curr.y = std::atoi(y.c_str());
        file >> str;
        str = str.substr(2);
        const auto [ox, oy] = split(str, ',');
        Pos vel;
        vel.x = std::atoi(ox.c_str());
        vel.y = std::atoi(oy.c_str());

        vec.push_back(Robot{ curr, vel });
    }

    move(7415);

    unsigned a = 0;
    unsigned b = 0;
    unsigned c = 0;
    unsigned d = 0;

    for (int i = 0; i < height; ++i)
    {
        for (int j = 0; j < width; ++j)
        {
            if (i < middleY and j < middleX)
            {
                a += matrix[i][j];
            }
            if (i < middleY and j > middleX)
            {
                b += matrix[i][j];
            }
            if (i > middleY and j > middleX)
            {
                c += matrix[i][j];
            }
            if (i > middleY and j < middleX)
            {
                d += matrix[i][j];
            }
        }
    }
}