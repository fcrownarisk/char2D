#!/usr/bin/env python3
"""
Octopus Simulator - 模拟章鱼的智能、灵活性和适应性
Python的灵活性和丰富的库完美匹配章鱼的多才多艺
"""

import random
import time
from dataclasses import dataclass
from enum import Enum
from typing import List, Dict, Optional

class CamouflagePattern(Enum):
    """章鱼的伪装模式"""
    ROCK = "rock"
    CORAL = "coral" 
    SAND = "sand"
    ALGAE = "algae"
    DANGER = "danger_flash"

class ProblemSolvingSkill(Enum):
    """章鱼的问题解决技能"""
    OPEN_JAR = "open_jar"
    USE_TOOL = "use_tool"
    ESCAPE = "escape"
    DECEIVE = "deceive"

@dataclass
class Octopus:
    """章鱼类 - 体现Python的灵活性和动态特性"""
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
        """改变伪装 - 体现Python的动态特性"""
        print(f"🐙 {self.name} 正在改变伪装为: {pattern.value}")
        time.sleep(1)  # 模拟伪装过程
        
        if pattern == CamouflagePattern.DANGER:
            color_changes = ["红色", "白色", "斑纹"]
            for color in color_changes:
                print(f"  闪烁: {color}")
                time.sleep(0.3)
        
        return f"伪装为{pattern.value}完成"
    
    def solve_problem(self, problem: str) -> Dict:
        """解决问题 - 体现Python的智能和适应性"""
        print(f"🔍 {self.name} 正在解决: {problem}")
        
        # 动态选择解决方案 - Python的灵活性
        solutions = {
            "maze": "使用空间记忆导航",
            "jar": "用触手旋转瓶盖",
            "predator": "喷墨并伪装",
            "puzzle": "试用不同方法"
        }
        
        solution = solutions.get(problem, "观察并学习新方法")
        
        # 模拟学习过程
        learning_rate = min(1.0, self.intelligence + 0.1)
        success_probability = learning_rate * 0.9
        
        return {
            "problem": problem,
            "solution": solution,
            "success_probability": success_probability,
            "tools_used": random.sample(self.tentacles, 2)
        }
    
    def explore_environment(self, environment: str) -> None:
        """探索环境 - 体现Python的易用性和表达力"""
        self.current_environment = environment
        print(f"🌊 {self.name} 正在探索 {environment}")
        
        # 使用列表推导式 - Pythonic的方式
        observations = [
            f"发现{item}" for item in 
            ["珊瑚", "贝壳", "小鱼", "洞穴", "海藻"]
        ]
        
        # 随机探索行为
        actions = [
            "用触手触摸物体",
            "改变颜色融入环境", 
            "收集贝壳作为工具",
            "记住地形特征"
        ]
        
        for observation, action in zip(observations[:3], random.sample(actions, 3)):
            print(f"  {observation} -> {action}")
            time.sleep(0.5)
    
    def __repr__(self):
        return f"Octopus(name='{self.name}', intelligence={self.intelligence}, age={self.age})"

def main():
    """主函数 - 演示章鱼的智能行为"""
    print("=== Python 章鱼模拟器 ===\n")
    
    # 创建一只聪明的章鱼 - 简单的初始化
    octopus = Octopus(
        name="奥克塔维亚",
        age=2,
        intelligence=0.85,
        camouflage_ability=0.95
    )
    
    print(f"创建章鱼: {octopus}")
    print(f"触手数量: {len(octopus.tentacles)}")
    
    # 演示各种能力
    octopus.explore_environment("深海珊瑚礁")
    print()
    
    # 伪装演示
    octopus.change_camouflage(CamouflagePattern.CORAL)
    print()
    
    # 问题解决演示
    problems = ["jar", "maze", "predator"]
    for problem in problems:
        result = octopus.solve_problem(problem)
        print(f"  解决方案: {result['solution']}")
        print(f"  成功率: {result['success_probability']:.1%}")
        print()

if __name__ == "__main__":
    main()