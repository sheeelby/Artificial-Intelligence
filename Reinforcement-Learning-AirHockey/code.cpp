#define _USE_MATH_DEFINES
#include <cmath>
#include <math.h>
#include <ctime>
#include <random>
#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <numeric>
#include "SFML/Graphics.hpp"
#include "matplotlibcpp.h"

template <class T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& s)
{
    out << "[ ";
    for (int i = 0; i < s.size(); i++)
        out << s[i] << "; ";
    out << "] ";
    return out;
}

// ��������� ��� ����������� ���������� ����
const int windowWidth = 600;
const int windowHeight = 800;
const float puckHeight = 30.0f;
const float sliderWidth = 100.0f;
const float sliderHeight = 15.0f;
const float puckSpeed = 10000.0f;
const float goalWidth = 200.0f; // ������ �����
const float borderWidth = 5.0f; // ������ ������� ���� � ��������
const float circleRadius = 80.0f; // ������ ������� ����������

float randomValue(float left = 2500000.0f, float right = 15000000.0f) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<float> distribution(left, right);
    return distribution(gen);
}

class GameRenderer {
protected:
    sf::RenderWindow& window;
    sf::RectangleShape field;
    sf::RectangleShape midLine;
    sf::CircleShape centerCircle;
    sf::CircleShape centerDot;
    sf::CircleShape lowerSemiCircle; // ����� �������������� � �����
    sf::CircleShape upperSemiCircle; // ������ �������������� � ����� 
    sf::RectangleShape blueSlider;
    sf::RectangleShape redSlider;
    sf::RectangleShape puck;
    sf::RectangleShape ourGoal;
    sf::RectangleShape enemyGoal;
    float blueSliderVelocity;
    float puckVelocity;
    sf::Vector2f puckDirection;

public:
    GameRenderer(sf::RenderWindow& window, float alpha) : window(window), blueSliderVelocity(randomValue()),
        puckVelocity(puckSpeed), puckDirection(cos(alpha), -sin(alpha)) {
        // ������������� �������� ����
        field.setSize(sf::Vector2f(windowWidth - 2 * borderWidth, windowHeight - 2 * borderWidth));
        field.setPosition(borderWidth, borderWidth);
        field.setFillColor(sf::Color::White);
        field.setOutlineThickness(borderWidth);
        field.setOutlineColor(sf::Color(135, 206, 250)); // Light Sky Blue

        // ������������� ����� ���������� ����
        midLine.setSize(sf::Vector2f(windowWidth - 2 * borderWidth, 5.0f));
        midLine.setPosition(borderWidth, windowHeight / 2);
        midLine.setFillColor(sf::Color(135, 206, 250)); // Light Sky Blue

        centerCircle.setRadius(circleRadius);
        centerCircle.setOutlineThickness(borderWidth); // ������� �����, ��� � ����������� �����
        centerCircle.setOutlineColor(sf::Color(135, 206, 250)); // ����, ��� � ����������� �����
        centerCircle.setFillColor(sf::Color::White); // ���������� ����������
        centerCircle.setPosition(windowWidth / 2 - circleRadius, windowHeight / 2 - circleRadius);

        centerDot.setRadius(puckHeight);
        centerDot.setFillColor(sf::Color(135, 206, 250)); // ����, ��� � ����������� �����
        centerDot.setPosition(windowWidth / 2 - puckHeight, windowHeight / 2 - puckHeight);

        float semiCircleRadius = goalWidth / 2; // ������ �������������� � �����

        // ����� ��������������
        lowerSemiCircle.setRadius(semiCircleRadius);
        lowerSemiCircle.setOutlineThickness(borderWidth); // ������� �����
        lowerSemiCircle.setOutlineColor(sf::Color(135, 206, 250)); // ���� �����
        lowerSemiCircle.setFillColor(sf::Color::Transparent); // ���������� ����������
        lowerSemiCircle.setPosition(windowWidth / 2 - semiCircleRadius, -semiCircleRadius);

        // ������ ��������������
        upperSemiCircle.setRadius(semiCircleRadius);
        upperSemiCircle.setOutlineThickness(borderWidth); // ������� �����
        upperSemiCircle.setOutlineColor(sf::Color(135, 206, 250)); // ���� �����
        upperSemiCircle.setFillColor(sf::Color::Transparent); // ���������� ����������
        upperSemiCircle.setPosition(windowWidth / 2 - semiCircleRadius, windowHeight - semiCircleRadius);

        // ������������� ���������
        blueSlider.setSize(sf::Vector2f(sliderWidth, sliderHeight));
        blueSlider.setPosition(windowWidth / 2 - sliderWidth / 2, windowHeight / 5);
        blueSlider.setFillColor(sf::Color::Blue);

        redSlider.setSize(sf::Vector2f(sliderWidth, sliderHeight));
        redSlider.setPosition(windowWidth / 2 - sliderWidth / 2, windowHeight * 4 / 5);
        redSlider.setFillColor(sf::Color::Red);

        // ������������� �����
        puck.setSize(sf::Vector2f(puckHeight, puckHeight));
        puck.setPosition(windowWidth / 2 - puckHeight / 2, windowHeight * 4 / 5 - sliderHeight * 2);
        puck.setFillColor(sf::Color::Black);

        // ������������� �����
        ourGoal.setSize(sf::Vector2f(goalWidth, borderWidth));
        ourGoal.setFillColor(sf::Color::White);
        enemyGoal.setSize(sf::Vector2f(goalWidth, borderWidth));
        enemyGoal.setFillColor(sf::Color::White);
    }

    bool isBorder(float puckX, float puckY, float SliderX, float SliderY) {
        return (SliderX <= puckX && puckX <= SliderX + goalWidth - puckHeight) &&
            (SliderY - puckHeight <= puckY && puckY <= SliderY + borderWidth);
    }

    bool isRedSlider(float deltaTime)
    {
        sf::Vector2f puckCenter = puck.getPosition() + sf::Vector2f(puckHeight / 2, puckHeight / 2);
        sf::Vector2f sliderCenter = redSlider.getPosition() + sf::Vector2f(sliderWidth / 2, sliderHeight / 2);
        sf::Vector2f distance = puckCenter - sliderCenter;
        // ������ �������� �����������
        if (std::abs(distance.x) < sliderWidth / 2 + puckHeight / 2 && std::abs(distance.y) < sliderHeight / 2 + puckHeight / 2) {
            // ���������� ��� ����������� (���� ������ ����� :P :o :3 8====D ) 
            float overlapX = sliderWidth / 2 + puckHeight / 2 - std::abs(distance.x);
            float overlapY = sliderHeight / 2 + puckHeight / 2 - std::abs(distance.y);
            return ((puck.getPosition().y > windowHeight / 2) && (puckDirection.y > 0) && (overlapX > overlapY));
        }
        return false;
    }

    int isGoal() {
        if (isBorder(puck.getPosition().x, puck.getPosition().y,
            ourGoal.getPosition().x, ourGoal.getPosition().y))
            return -1;
        if (isBorder(puck.getPosition().x, puck.getPosition().y,
            enemyGoal.getPosition().x, enemyGoal.getPosition().y))
            return 1;
        return 0;
    }

    void resetRound() {
        // ����� ������� �����
        puck.setPosition(windowWidth / 2 - puckHeight / 2, windowHeight * 4 / 5 - sliderHeight * 2);

        // ����� ������� ���������
        blueSlider.setPosition(windowWidth / 2 - sliderWidth / 2, windowHeight / 5);
        redSlider.setPosition(windowWidth / 2 - sliderWidth / 2, windowHeight * 4 / 5);

        // ����� �������� �����
        puckVelocity = puckSpeed;
        blueSliderVelocity = randomValue();
    }

    // ��������� ����������� ������������ ����� �� ���������
    void handlePuckSliderCollision(sf::RectangleShape& slider, float deltaTime) {
        sf::Vector2f puckCenter = puck.getPosition() + sf::Vector2f(puckHeight / 2, puckHeight / 2);
        sf::Vector2f sliderCenter = slider.getPosition() + sf::Vector2f(sliderWidth / 2, sliderHeight / 2);

        sf::Vector2f distance = puckCenter - sliderCenter;

        // ������ �������� �����������
        if (std::abs(distance.x) < sliderWidth / 2 + puckHeight / 2 && std::abs(distance.y) < sliderHeight / 2 + puckHeight / 2) {

            float overlapX = sliderWidth / 2 + puckHeight / 2 - std::abs(distance.x);
            float overlapY = sliderHeight / 2 + puckHeight / 2 - std::abs(distance.y);

            if (overlapX > overlapY) {
                // ����������� ������ ��� �����
                puckDirection.y = -puckDirection.y;
                // ������������ � ��������������� �����������
                float correction = (distance.y > 0) ? overlapY : -overlapY;
                puck.move(0, correction);
            }
            else {
                if (!((puck.getPosition().y < windowHeight / 2) && (puckDirection.x * blueSliderVelocity > 0)
                    && (abs(puckDirection.x) * puckSpeed < abs(blueSliderVelocity)))) {
                    //��������� �������� ����� � ����� ������� ����� ��� ����� �����������
                    puckDirection.x = -puckDirection.x;
                }
                // ������������ � ��������������� �����������
                float correction = (distance.x > 0) ? overlapX : -overlapX;
                puck.move(correction, 0);
            }
        }
    }

    void update(float deltaTime) {
        // ���������� ������ ��������
        if (blueSlider.getPosition().x <= borderWidth) {
            blueSliderVelocity = -blueSliderVelocity;
            blueSlider.setPosition(borderWidth + 1, blueSlider.getPosition().y); // ��������� ��������� ��������
        }
        else if (blueSlider.getPosition().x + sliderWidth >= windowWidth - borderWidth) {
            blueSliderVelocity = -blueSliderVelocity;
            blueSlider.setPosition(windowWidth - borderWidth - sliderWidth - 1, blueSlider.getPosition().y); // ��������� ��������� ��������
        }
        blueSlider.move(blueSliderVelocity * deltaTime, 0.0f);

        if (puck.getPosition().x <= borderWidth) {
            puckDirection.x = -1 * puckDirection.x;
            puck.setPosition(borderWidth + 1, puck.getPosition().y); // �������� ����� ������ ����
        }
        else if (puck.getPosition().x + puckHeight >= windowWidth - borderWidth) {
            puckDirection.x = -1 * puckDirection.x;
            puck.setPosition(windowWidth - borderWidth - puckHeight - 1, puck.getPosition().y); // �������� ����� ������ ����
        }

        // ��������� ��� ������������ � ��������� �� ��� Y
        if (puck.getPosition().y <= borderWidth) {
            puckDirection.y = -1 * puckDirection.y;
            puck.setPosition(puck.getPosition().x, borderWidth + 1); // �������� ����� ������ ����
        }
        else if (puck.getPosition().y + puckHeight >= windowHeight - borderWidth) {
            puckDirection.y = -1 * puckDirection.y;
            puck.setPosition(puck.getPosition().x, windowHeight - borderWidth - puckHeight - 1); // �������� ����� ������ ����
        }

        // ���������� ��������� ���������� � ����� ���������
        handlePuckSliderCollision(blueSlider, deltaTime);

        // ���������� ��������� ���������� � ������� ���������
        handlePuckSliderCollision(redSlider, deltaTime);

        puck.move(puckVelocity * deltaTime * puckDirection.x, puckVelocity * deltaTime * puckDirection.y);
    }

    void drawField() {
        window.draw(field);
        window.draw(midLine); // ��������� ����� ���������� ����
        window.draw(centerCircle);
        window.draw(lowerSemiCircle);
        window.draw(upperSemiCircle);
        window.draw(centerDot);
    }

    void drawGoals() {
        enemyGoal.setPosition(windowWidth / 2 - goalWidth / 2, 0);
        window.draw(enemyGoal);
        ourGoal.setPosition(windowWidth / 2 - goalWidth / 2, windowHeight - borderWidth);
        window.draw(ourGoal);
    }

    void drawSliders() {
        window.draw(blueSlider);
        window.draw(redSlider);
    }

    void drawPuck() {
        window.draw(puck);
    }

    void render() {
        window.clear();
        drawField();
        drawGoals();
        drawSliders();
        drawPuck();
        window.display();
    }

    // ������� � ������� ��� ��������, ���� ���������� ��������� ��������� ����� ������
    void setBlueSliderVelocity(float velocity) {
        blueSliderVelocity = velocity;
    }

    float getBlueSliderVelocity() const {
        return blueSliderVelocity;
    }

    void setPuckDirection(float alpha) {
        puckDirection = sf::Vector2f(cos(alpha), -sin(alpha));
    }
};

class Agent {
protected:
    std::vector<float> Actions;
    std::vector<float> Probabilities;
    std::vector<float> Q;
    std::vector<float> Reactions;
    std::vector<int> Counts;
    float exp_parameter;
    int num_rounds;
    void (*agentType)(std::vector<float>, std::vector<float>&, std::vector<int>, float, int);

public:
    Agent(std::vector<float> Actions, void(*agentType)(std::vector<float>, std::vector<float>&, std::vector<int>, float, int), float exp_parameter = 0.1f) : Actions(Actions), agentType(agentType), exp_parameter(exp_parameter) {
        Probabilities = std::vector<float>(Actions.size(), 1.0 / Actions.size());
        Q = std::vector<float>(Actions.size());
        Reactions = std::vector<float>(Actions.size());
        Counts = std::vector<int>(Actions.size());
        num_rounds = 0;
    }

    void QRecalc(int action) {
        Q[action] = Q[action] + 1.0 / (Counts[action] + 1) * (Reactions[action] - Q[action]);
    }

    int CooseAction() {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<> dis(0.0, 1.0);

        std::discrete_distribution<> dist(Probabilities.begin(), Probabilities.end());

        return dist(gen);
    }

    virtual void StrategyRecalc() { // ������� ��� ��������� ���������
        agentType(Q, Probabilities, Counts, exp_parameter, num_rounds);
    }

    friend void UCB(std::vector<float> Q, std::vector<float>& Probabilities, std::vector<int> Counts, float exp_parameter, int num_rounds);
    friend void SoftMax(std::vector<float> Q, std::vector<float>& Probabilities, std::vector<int> Counts, float exp_parameter, int num_rounds);
    friend void EpsilonGreedy(std::vector<float> Q, std::vector<float>& Probabilities, std::vector<int> Counts, float exp_parameter, int num_rounds);
    friend void Greedy(std::vector<float> Q, std::vector<float>& Probabilities, std::vector<int> Counts, float exp_parameter, int num_rounds);
    friend class Game;
};

void UCB(std::vector<float> Q, std::vector<float>& Probabilities, std::vector<int> Counts, float exp_parameter, int num_rounds) {
    std::vector<float> A_t(Q.size());

    for (int i = 0; i < Q.size(); i++)
        A_t[i] = (Q[i] + exp_parameter * std::sqrt(2.0 * std::log(num_rounds) / Counts[i]));

    float max_elem = *max_element(A_t.begin(), A_t.end());

    float cnt_max = std::count(A_t.begin(), A_t.end(), max_elem);

    for (int i = 0; i < A_t.size(); i++)
        Probabilities[i] = 1.0 / cnt_max * (max_elem == A_t[i]);
};

void SoftMax(std::vector<float> Q, std::vector<float>& Probabilities, std::vector<int> Counts, float exp_parameter, int num_rounds) {
    for (int i = 0; i < Q.size(); i++)
    {
        float sum = 0;
        for (int j = 0; j < Q.size(); j++)
            sum += (std::exp((1 / exp_parameter) * Q[j]));

        Probabilities[i] = (std::exp((1 / exp_parameter) * Q[i])) / sum;
    }
};

void EpsilonGreedy(std::vector<float> Q, std::vector<float>& Probabilities, std::vector<int> Counts, float exp_parameter, int num_rounds) {
    float max_elem = *max_element(Q.begin(), Q.end());

    float cnt_max = std::count(Q.begin(), Q.end(), max_elem);

    for (int i = 0; i < Q.size(); i++)
        Probabilities[i] = ((1 - exp_parameter) / cnt_max * (max_elem == Q[i])) + (exp_parameter / Probabilities.size());
};

void Greedy(std::vector<float> Q, std::vector<float>& Probabilities, std::vector<int> Counts, float exp_parameter, int num_rounds) {
    float max_elem = *max_element(Q.begin(), Q.end());

    float cnt_max = std::count(Q.begin(), Q.end(), max_elem);

    for (int i = 0; i < Q.size(); i++)
        Probabilities[i] = 1 / cnt_max * (max_elem == Q[i]);
};

float base_parametr_reculc(float parametr, int num_cycles) {
    return parametr;
}

float erasing_parametr_reculc(float parametr, int num_cycles) {
    if (num_cycles % 100 == 0 && num_cycles != 0 && parametr > 0.1f)
        return parametr - 0.05f;
    else
        return parametr;
}

class Game {
protected:
    Agent agent;
    sf::RenderWindow window;
    int num_rounds;
    float (*parametr_reculc)(float, int);
    std::vector<float> AvgReward;
    std::vector<float> rewards;
    int curaction;
    GameRenderer renderer;
    sf::Clock clock;

public:
    Game(Agent agent, int num_rounds = 2000, float (*parametr_reculc)(float, int) = base_parametr_reculc) :
        agent(agent), renderer(window, agent.Actions[0]), num_rounds(num_rounds), parametr_reculc(parametr_reculc) {
        curaction = 0;
        window.create(sf::VideoMode(windowWidth, windowHeight), "Air Hockey");
    }

    void Play(int num_game = 0) {
        // ������� ���� ����
        while (window.isOpen()) {

            sf::Event event;
            while (window.pollEvent(event)) {
                if (event.type == sf::Event::Closed)
                    window.close();
            }

            if (agent.num_rounds >= num_rounds)
                window.close();

            // ���������� ���������� �������
            sf::Time elapsed = clock.restart();
            float deltaTime = elapsed.asSeconds();

            int flag = renderer.isGoal();
            int flag1 = renderer.isRedSlider(deltaTime);
            // �������� �� ���
            if (flag || flag1) {

                agent.Reactions[curaction] = flag;
                agent.QRecalc(curaction);
                agent.Counts[curaction] += 1;

                agent.num_rounds++;

                agent.exp_parameter = parametr_reculc(agent.exp_parameter, agent.num_rounds);

                float sum = 0;

                for (int i = 0; i < agent.Q.size(); i++)
                    sum += agent.Q[i] * agent.Counts[i];

                AvgReward.push_back(sum / 10);

                rewards.push_back(agent.Reactions[curaction]);

                agent.StrategyRecalc();

                curaction = agent.CooseAction();
                renderer.setPuckDirection(agent.Actions[curaction]);


                std::cout << "\nGame: " << num_game << "\nCycle: " << agent.num_rounds << "\nInterpreter: " << agent.exp_parameter
                    << "\nCounts: " << agent.Counts << "\nProbabilities: " << agent.Probabilities
                    << "\nQ: " << agent.Q << "\nReactions: " << agent.Reactions << "\n";

                renderer.resetRound(); // ������������� �����
            }
            // ���������� ����
            renderer.update(deltaTime);

            // ��������� �����
            renderer.render();
        }
    }

    std::vector<float> GetRewards() {
        return rewards;
    }
};

class Agent_Statistics {
protected:
    std::vector<std::vector<float>> reward_histories;
    void (*agentType)(std::vector<float>, std::vector<float>&, std::vector<int>, float, int);
    float exp_parameter;
    int num_rounds;
    int num_games;

    std::vector<float> choose_actions() {
        std::random_device rd;
        std::mt19937 generator(rd());

        std::vector<float> numbers; // ������ �������� 18 (0.05 �� 0.95 � ����� 0.05)

        // ���������� std::iota ��� ���������� �������
        for (float i = 0.05f; i < 1; i += 0.05f)
            numbers.push_back(i);

        // ������������� �������
        std::shuffle(numbers.begin(), numbers.end(), generator);

        std::vector<float> ans;
        for (int i = 0; i < 10; ++i)
            ans.push_back(numbers[i] * M_PI);

        return ans;
    }

public:
    Agent_Statistics(void (*agentType)(std::vector<float>, std::vector<float>&, std::vector<int>, float, int), float exp_parameter,
        int num_rounds, int num_games) :
        agentType(agentType), exp_parameter(exp_parameter), num_rounds(num_rounds), num_games(num_games) { }

    std::vector<float> GetAvgReward() {

        for (int i = 0; i < num_games; i++) {

            std::vector<float> acts = choose_actions();

            Agent agent(acts, agentType, exp_parameter);
            Game game(agent, num_rounds);

            game.Play(i);
            reward_histories.push_back(game.GetRewards());
        }

        std::vector<float> ans;

        for (int i = 0; i < num_rounds; i++) {
            float sum = 0;
            for (int j = 0; j < num_games; j++)
                sum += reward_histories[j][i];
            ans.push_back(sum / num_games);
        }

        return ans;
    }
};

int main() {

    std::vector<float> interpret;

    interpret.push_back(100.0f); interpret.push_back(50.0f); interpret.push_back(1.0f);
    interpret.push_back(0.5f); interpret.push_back(0.1f);

    for (int i = 0; i < interpret.size(); i++) {

        std::cout << interpret[i] << "\n";

        Agent_Statistics agent1(SoftMax, interpret[i], 500, 2000);
        std::vector<float> episode1_rewards = agent1.GetAvgReward();

        std::vector<float> episodes(episode1_rewards.size());
        std::iota(episodes.begin(), episodes.end(), 1);

        std::cout << episode1_rewards << "\n\n\n";
    }

    return 0;
}