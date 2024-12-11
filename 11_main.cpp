#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <unordered_map>

using Int = long long unsigned;

struct Foo
{
    Int val;
    Int blinks;
    bool operator==(const Foo &other) const
    {
        return val == other.val && blinks == other.blinks;
    }
};

struct FooHash
{
    std::size_t operator()(const Foo &foo) const
    {
        std::size_t h1 = std::hash<Int>()(foo.val);
        std::size_t h2 = std::hash<Int>()(foo.blinks);
        return h1 ^ (h2 << 1);
    }
};

std::unordered_map<Foo, Int, FooHash> map;

Int simulate(Int stone, Int blinks)
{
    Foo f{stone, blinks};
    if(map.count(f) > 0)
    {
        return map[f];
    }

    if(blinks == 1)
    {
        if(std::to_string(stone).size() % 2 == 0)
        {
            return 2;
        }
        return 1;
    }
    else
    {
        if(stone == 0)
        {
            Int res = simulate(1, blinks - 1);
            map[f] = res; 
            return res;
        }
        
        std::string str = std::to_string(stone);
        if(str.size() % 2 == 0)
        {
            Int res = simulate(std::atoi(str.substr(0, str.size() / 2).c_str()), blinks - 1) + simulate(std::atoi(str.substr(str.size() / 2).c_str()), blinks - 1);
            map[f] = res; 
            return res;
        }
        else
        {
            Int res = simulate(stone * 2024, blinks - 1);
            map[f] = res; 
            return res;
        }
    }

    return -1;
}


int main() 
{
    std::fstream file("data.txt");
    Int sum = 0;
    Int sum_2 = 0;
    while(file.good())
    {
        Int x;
        file >> x;
        Int res = simulate(x, 25);
        sum += res;
        res = simulate(x, 75);
        sum_2 += res;
    }

    std::cout << "Part_1: " << sum << '\n';
    std::cout << "Part_2: " << sum_2 << '\n';

    return 0;
}
