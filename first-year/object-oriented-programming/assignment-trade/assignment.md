# Bajt Trade Assignment

## Task Objective

The goal of this task is to create a simulation of a market in which Agents participate, aiming to accumulate the highest number of diamonds. There are two main types of Agents: Workers and Speculators. If there are any doubts, please refer to the assumptions section.

## Simulation Process

The simulation will take place in turns (also called days). The simulation starts with turn number 1. Each turn consists of the following stages performed in the specified order. Each stage is described in detail in the respective section:

1. **Workers decide whether to spend the day learning or working.**

   - a. **Worker learns.** After learning, the worker ends the day (none of the points below apply).
   - b. **Worker works.**
     - i. **Worker produces items based on their strategies.**
     - ii. **Worker lists products they produced for sale on the market.** Worker sale offers do not have prices, only the number and type of items they want to sell.
     - iii. **Worker lists purchase offers.** Similarly, purchase offers do not have prices, only the type and number of items.

2. **Speculators enter the market with their purchase and sale offers.** Their offers include prices in addition to the number and type of products.

3. **The market matches purchase offers with sale offers.** Only Worker offers are matched with Speculator offers, setting the transaction price. Thus, a Worker will not sell/buy anything to/from another Worker, and the same applies to Speculators.

4. **After executing matched transactions, the market buys unsold products from Workers.** Unfulfilled Speculator offers (purchase and sale) and Worker purchase offers are cleared from the market's memory. Each turn, the market starts without any offers.

5. **At the end of the day, Workers consume their items:** food, clothes, tools, and used software programs.

Note: Learning Agents do not consume any items at the end of the turn, including food.

## Workers

Each Worker starts the simulation with a set number of products (read from input). Additionally, each Worker has a fixed base productivity vector, which specifies how many units of each product the Worker can produce in one turn. The numbers in this vector will always be multiples of 100. During the simulation, this value will be modified by various bonuses (expressed as whole percentage points, e.g., +10%, +20%, -1%, -150%), so Workers will always produce a whole number of products per turn. If the bonuses (which can be negative) result in a negative number of products, no products are produced. For example, if the base production of tools is 200 and the bonuses are -10%, +20%, +40%, then in that turn, the Worker will produce 200 + 200 \* (20 + 40 - 10)% = 200 + 100 = 300 tools.

In each turn, a Worker can either learn or work. Learning involves progressing in the current career path or changing the career path. Working involves producing, selling, and buying products. If a Worker works in a given turn, they also consume (lose) items as follows:

- Consumes 100 units of food. If they have less, they consume what they have, but it counts as if they did not eat that day.
- Consumes all their tools.
- Adds one day of wear to any 100 clothes. The remaining clothes (if any) remain unchanged. An item of clothing is fully worn out if it has been worn for as many days as its durability. If the Worker has fewer than 100 clothes, one day of wear is added to all they have.
- Consumes the software programs used for production that day. The remaining programs stay unchanged.

### When does a Worker learn, and when do they work?

Each Worker follows one of the following strategies that determine what they do each day:

- **Workaholic** never learns, always works.
- **Frugal** learns if and only if they have more than `limit_diamonds` diamonds, where `limit_diamonds` is a strategy parameter.
- **Student** learns if and only if they can afford to buy 100 \* `reserve` units of food at a price equal to the arithmetic mean of average prices over the last `period` days, where `reserve` and `period` are strategy parameters.
- **Periodic** learns every `learning_period` days, where `learning_period` is a strategy parameter. On other days, they work. For example, for `learning_period` = 10, they will learn on days 10, 20, 30, etc.
- **Stochastic** works with a probability of 1 - 1/(simulation_day + 3), and learns with a probability of 1/(simulation_day + 3).

### How does a Worker learn?

A Worker learns in one of two ways: either by advancing in the current career path or by changing the career path. After changing the career path, the Worker does not forget the levels of previous paths. They can only use the productivity bonuses of the current path, but if they switch back to a previously held path, they resume at the level they had. For example, a Level 3 Farmer who becomes a Miner starts as a Level 1 Miner (if they have never been a Miner before). If they later switch back to being a Farmer, they resume at Level 3.

Workers follow one of two strategies for changing career paths:

- **Conservative** never changes career paths.
- **Revolutionary** every 7 days calculates `n` as max(1, their id modulo 17). They then choose the career path giving the bonus for the product that appeared most frequently (in terms of total units in sale offers by Speculators and Workers) over the last `n` days. On other days, they do not change career paths. If they choose a career path they already have, they instead advance in it. For example, they do not change paths on days 1-6 and choose a potential new path on day 7. They do not change paths on days 8-13 and choose a potential new path on day 14, etc.

### What does a Worker produce?

What a Worker produces is determined by their production strategy. There are several such strategies:

- **Short-sighted** always produces the product with the highest average price the previous day.
- **Greedy** always produces the product that will give the highest profit that day. Profit is calculated by multiplying the number of products they would produce by the previous day's average price.
- **Average** always produces the product with the highest maximum average price over the last `production_history_days`, where `production_history_days` is a worker parameter.
- **Perspective** always produces the product with the highest price increase over the last `perspective_history_days`, where `perspective_history_days` is a worker parameter. Price increase is the difference between the current average price and the average price `perspective_history_days` ago.
- **Random** always produces a random product.

The number of produced items is determined by the Worker’s base productivity vector and the productivity bonus. For example, if they produce Diamonds with a base of 100 per day and have a +50% bonus, they will produce 100 + 50% \* 100 = 150 per day.

A Worker always produces one type of product per day.

### What does a Worker sell?

A Worker sells everything they produced that day except diamonds. If they bought something earlier (or had it initially) and it remains, they do not sell it. Note that this means a Worker cannot eat food, wear clothes, use tools, or use software programs they produced, as the market buys all Worker sale offers at the end of the day.

### What does a Worker buy?

A Worker can have one of four strategies for buying and using software programs:

- **Technophobe** buys only 100 units of food daily.
- **Neat** buys 100 units of food daily and ensures they have at least 100 clothes for the next turn (considering wear and tear at the end of this turn).
- **Mechanized** buys 100 units of food, `number_of_tools` (where `number_of_tools` is a strategy parameter) tools daily, and takes care of clothes like the Neat strategy.
- **Gadgeteer** buys 100 units of food, `number_of_tools` (where `number_of_tools` is a strategy parameter) tools daily, and takes care of clothes like the Neat strategy. Additionally, they always buy as many programs as they produced products in the current round. Programs are used immediately in order of highest level. For example, if they have 5 level 2 programs and 3 level 1 programs, and they produce 6 tools, they will produce 6 tools: five at quality level 2 and one at quality level 1. At the end of the day, they will have 2 level 1 programs left.

## Career Paths

The available career paths are:

- Farmer
- Miner
- Craftsman
- Engineer
- Programmer

Each career path can have any positive level, which provides a production bonus.

### Level Bonuses

| Level | Bonus                                                                                |
| ----- | ------------------------------------------------------------------------------------ |
| 1     | +50% of base value                                                                   |
| 2     | +150% of base value                                                                  |
| 3     | +300% of base value                                                                  |
| >3    | +300% + cumulative +25% for each level above 3. Percentages apply to the base value. |

The production bonus is applied only to the product associated with the current career path:

| Career Path | Product |
| ----------- | ------- |
| Farmer      | Food    |

|

Craftsman | Clothes |
| Engineer | Tools |
| Miner | Diamonds |
| Programmer | Software Programs |

## Products

The following products are available in the simulation:

- Diamonds
- Food
- Clothes
- Tools
- Software Programs

Each product has its unique uses:

- **Food**: Essential for every working Worker; each Worker consumes 100 units of food daily. If a Worker did not eat the previous day, they get a -100% productivity penalty; if they did not eat for two consecutive days, the penalty is -300%. After three days without food, the Worker dies. A dead Worker performs no actions, and their game score is zero diamonds. If a Worker learned that day, we assume they ate in the cafeteria for free, resetting the counter of days without eating.
- **Clothes**: (have quality levels) Having fewer than 100 clothes at the start of a turn causes the Worker to get a productivity penalty equal to -`lack_of_clothes_penalty`%, where `lack_of_clothes_penalty` is a simulation parameter. Clothes wear out after `y^2` turns of use, where `y` is the quality level of the clothes.
- **Tools**: (have quality levels) Each tool gives a +`y`% productivity bonus (if there are multiple tools, the bonuses are cumulative), where `y` is the quality level of the tool. Tools are entirely consumed after one turn of production.
- **Diamonds**: The ultimate currency in the game.
- **Software Programs**: (have advancement levels) Allow setting the quality level of produced tools and clothes to the program's advancement level. If the Worker is a Programmer, the program's advancement level equals their career path level; otherwise, it is 1. Each program applies to one tool or clothing item. Programs are single-use, so they are consumed after use.

## Market Description

The market features purchase and sale offers (with prices and quantities from Speculators and without prices but with quantities from Workers). Diamonds cannot be bought or sold on the market! For software programs, tools, and clothes, there are separate offers for different advancement levels. The exception is a Worker's purchase offer for programs—they want to buy a number of programs, tools, or clothes at the highest quality/advancement level available.

The market operates according to one of three strategies:

- **Capitalist**: First fulfills transactions for Workers with the most diamonds.
- **Socialist**: The reverse order.
- **Balanced**: Alternates between socialist and capitalist strategies each turn.

If there is a tie in the number of diamonds, the Worker’s ID determines the order. In other words, pairs (number of diamonds, Worker ID) are compared in ascending or descending order alternately.

When it is a Worker's turn, they transact according to the most advantageous offers available. A Worker fulfills all their sell offers first, followed by all their buy offers. Speculator and Worker offers can be partially fulfilled. The order of selling and buying products for a Worker is always the same: Food, Clothes, Tools, Software Programs. For software programs, clothes, and tools, Speculator sale offers are sorted as pairs (advancement/quality level descending, price ascending). This means a higher-level program is always a better offer regardless of the price, and for programs of the same level, the price determines the offer’s advantage.

For example, if a Worker wants to sell 5 units of food and the Speculator buy offers are (1 food for 3 diamonds), (100 food for 4 diamonds), and (3 food for 5 diamonds), the Worker will earn 5 _ 3 + 4 _ 2 = 23 diamonds. The remaining offers on the market will be (1 food for 3 diamonds) and (98 food for 4 diamonds). If the same Worker wants to buy 4 tools (without specifying the level), and the Speculator offers are (5 level 3 tools for 29 diamonds) and (100 level 2 tools for 1 diamond), and assuming the Worker had 7 diamonds initially, they would have 30 diamonds after selling food. They can only afford 2 tools: one level 3 (first buys the best) and one level 2 tool. Thus, the remaining offers will be (4 level 3 tools for 29 diamonds) and (99 level 2 tools for 1 diamond). The next Worker in line will then transact.

If a Worker fails to sell what they wanted (because Speculator buy offers are exhausted), the remaining products are bought by the bank at the lowest buy price for that product from the previous day (turn) or the zero-turn price if there were no buy offers for that product the previous day. Unfulfilled Speculator buy and sell offers and Worker buy offers disappear and do not carry over to the next turn.

## Speculators

The second type of Agent is the Speculator. Instead of producing, they trade on the market. They produce nothing but have a set budget of diamonds per turn, which they can use to buy items. Unused budget is wasted and fully replenishes at the start of the next turn. Initially, Speculators have no items.

Each Speculator, if they list a buy offer, buys 100 units of a product (software programs, clothes, and tools of different levels are considered different products). If they list a sell offer, they sell everything they have in one offer. Speculators have different trading strategies (the same for each type of product and level):

- **Average Speculators** list buy and sell offers 10% below and 10% above the arithmetic mean of the average prices from the last `average_speculator_history` (a Speculator parameter) days. If they do not have a product, they list only a buy offer 5% below this value. Average Speculators sell everything they have each turn (except diamonds, see market description).
- **Convex Speculators** buy only if the function formed by the average prices of the last 3 days is strictly convex and sell if it is strictly concave. Otherwise, they do nothing. The buy and sell prices are respectively 10% below and 10% above the average price from the last day.
- **Market Regulator Speculators** do nothing in the first turn. In subsequent turns, they take the previous day's average price and multiply it by the current turn number and `p_i/max(p_{i-1}, 1)`, where `p_i` is the number of products listed for sale by Workers in turn i. They then list a buy offer 10% below this price and a sell offer 10% above it.

## JSON

To handle JSON read and write, you can use the Moshi library: [Moshi GitHub](https://github.com/square/moshi) or Gson.

## Additional Assumptions and Definitions

- For any backward-looking x turns: If fewer than x full turns have passed since the simulation started, we simply take as many as there are. The input will include the costs of all products (except diamonds) in the "zero" turn. We interpret this as if in the "zero" turn there was one Speculator buy offer for each product (1 product for ... diamonds). The price for different levels of software programs is the same for all in the "zero" turn. The same applies to the price for different levels of clothes/tools. For example, the average of 10 turns needed for a decision on turn 5 is the average of the first 5 turns (4 real and 1 zero), similarly for calculating convex/concave functions. If calculating the average for the last 5 days in turn 100, we only take the last 5 days, ignoring the zero turn.
- Assume diamonds are perfectly divisible like bitcoins.
- Worker IDs are unique.
- The average price of a product on a given day is the weighted average of the prices with the weights being the number of items. We consider all fulfilled Speculator buy and sell offers. If no offers were fulfilled for a product, the average price is the zero-turn price. For example, if we have offers (5 clothes for 1 diamond) and (2 clothes for 3 diamonds), the average price of clothes that day is (5 _ 1 + 2 _ 3) / 7.
- Items held by agents at the start of the simulation (provided in the input) are treated as level 1 (for clothes, tools, and programs) and as purchased. This means they are not sold, only used.

## FAQ

- **Does a Worker consume food/tools/clothes/software programs on a day they learn?** No.
- **What happens to unused software programs?** For example, producing 10 tools but having 100 programs? At the end of the turn, the Worker loses 10 programs (since they used them for tool production), but keeps the remaining 90 for future use.
- **What exactly does "consuming" an item mean?** It means that the item should be removed from the simulation when used. There are no "consumed" items, all such items are discarded.
- **What to do when choosing a product/career path in case of a tie (according to the established measure)?** Ties are resolved by considering the order of career paths/products in the table. For example, if we are to produce and according to our strategy Tools and Software Programs are equally good, we produce Software Programs as they are further down in the table. Similarly, if we are to change the career path and Farmer and Craftsman are equally good, we choose the latter, i.e., Craftsman.
