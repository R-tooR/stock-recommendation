# stock-recommendation
Implementation of real-time recommendation system for Master Thesis entitled "A stock recommendation system based on graph knowledge representation".
To run, it needs working Neo4j database instance.

Project contains:
- data-server: for simulation of incoming data, and processing them. The module calculates ratio of stock basing on technical analysis of parameters
- recommendation-system: fetches investors from database, calculates similarity between them, and combine this with output from data-server to recommend stocks
  which can be in interest of target investor, and have good predictions for closest future.

To run:
- setup Neo4j database
- (only in IntelliJ IDEA): create configuration for main classes in both modules, then run first "data-server" then "recommendation-system" (you can do it using Compound configuration in IDE)
