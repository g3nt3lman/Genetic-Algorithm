# Genetic-Algorithm


The aim of this project is to optimize the television advertising campaign, so that with a limited budget, the advertiser could reach as many customers from his target group as possible. Size of an audience can be counted in several ways, here it has been measured by the amount of GRP that the found solution provides, i.e. the sum of the viewership of all advertising broadcasts among the target group.

In this work, the optimization was carried out for a TV campaign, because advertisers in this medium spent 2,204 million PLN in 2018 what was about 47% of the total marketing budgets. For this and many other reasons, the author decided to focus on television, because it is the most important of all available media. Real data on TV viewing in Poland were used in the calculations in four main channels such as TVN, Polsat, TVP1 and TVP2.

The aim of the project was realized through the implementation of the Genetic Algorithm, which is a kind of heuristic that belongs to the group of Evolutionary Algorithms. This method imitates the genetic evolution of living organisms in order to search the space of potential solutions to a given problem.

Data was acquired thanks to Nielsen Poland company from the 'Nielsen People Meter' device. Company also allowed to use Arianna program where viewing rates predictions were made. The comparison of solutions found by the Arianna program (which is used for every day optimization of TV advertising campaigns), with solutions found by the Genetic Algorithm confirms the effectiveness of the method used in this work to solve given optimization problem.

GA is known as a method that sometimes stuck in local optima. To prevent this kind of situation there waere implemented two different method - "Judgement Day" and "No Penalty".
