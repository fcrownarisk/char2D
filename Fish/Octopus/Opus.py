
import random
import time
from dataclasses import dataclass
from enum import Enum
from typing import List, Dict, Optional

class CamouflagePattern(Enum):
    ROCK = "rock"
    CORAL = "coral" 
    SAND = "sand"
    ALGAE = "algae"
    DANGER = "danger_flash"

class ProblemSolvingSkill(Enum):
    OPEN_JAR = "open_jar"
    USE_TOOL = "use_tool"
    ESCAPE = "escape"
    DECEIVE = "deceive"

@dataclass
class Octopus:
    name: str
    age: int
    intelligence: float  # 0.0 to 1.0
    camouflage_ability: float
    tentacles: List[str] = None
    current_environment: str = "coral_reef"
    
    def __post_init__(self):
        if self.tentacles is None:
            self.tentacles = [f"tentacle_{i}" for i in range(8)]
    
    def change_camouflage(self, pattern: CamouflagePattern) -> str:
        print(f"🐙 {self.name} : {pattern.value}")
        time.sleep(1) 
        
        if pattern == CamouflagePattern.DANGER:
            color_changes = [".", "..", "..."]
            for color in color_changes:
                print(f" : {color}")
                time.sleep(0.3)
        
        return f"{pattern.value}"
    
    def solve_problem(self, problem: str) -> Dict:
        print(f"🔍 {self.name} 正在解决: {problem}")
        
        solutions = {
            "maze": "position",
            "jar": "rotate",
            "predator": "pretend",
            "puzzle": "method"
        }
        
        solution = solutions.get(problem, "")
        
        learning_rate = min(1.0, self.intelligence + 0.1)
        success_probability = learning_rate * 0.9
        
        return {
            "problem": problem,
            "solution": solution,
            "success_probability": success_probability,
            "tools_used": random.sample(self.tentacles, 2)
        }
    
    def explore_environment(self, environment: str) -> None:
        self.current_environment = environment
        print(f"🌊 {self.name} 正在探索 {environment}")
        
        observations = [
            f"发现{item}" for item in 
            ["珊瑚", "贝壳", "小鱼", "洞穴", "海藻"]
        ]
        
        actions = [

        ]
        
        for observation, action in zip(observations[:3], random.sample(actions, 3)):
            print(f"  {observation} -> {action}")
            time.sleep(0.5)
    
    def __repr__(self):
        return f"Octopus(name='{self.name}', intelligence={self.intelligence}, age={self.age})"

def main()
    
    octopus = Octopus(
        name="",
        age=2,
        intelligence=0.85,
        camouflage_ability=0.95
    )
    
    octopus.explore_environment("Deep Ocean")
    print()

    octopus.change_camouflage(CamouflagePattern.CORAL)
    print()
    problems = ["jar", "maze", "predator"]
    
    for problem in problems:
        result = octopus.solve_problem(problem)
        print(f" : {result['solution']}")
        print(f" : {result['success_probability']:.1%}")
        print()

if __name__ == "__main__":

    main()
