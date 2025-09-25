package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

/*
Squid Simulator - 模拟鱿鱼的高速游泳和群体行为
Go的并发性能和简洁语法完美匹配鱿鱼的高效特性
*/

// Squid 结构体 - Go的简洁结构定义
type Squid struct {
	Name          string
	Age           int
	Speed         float64 // 游泳速度 m/s
	JetPower      float64 // 喷水推进力
	School        *School
	Position      Position
	IsSwimming    bool
}

// Position 位置信息
type Position struct {
	X, Y, Z float64
}

// School 鱼群 - 体现Go的并发和群体概念
type School struct {
	Name     string
	Members  []*Squid
	sync.RWMutex
}

// 常量定义 - Go的显式类型定义
const (
	MaxSpeed    = 10.0
	MaxJetPower = 8.0
)

// NewSquid 创建新鱿鱼 - Go的构造函数模式
func NewSquid(name string, age int, speed, jetPower float64) *Squid {
	if speed > MaxSpeed {
		speed = MaxSpeed
	}
	if jetPower > MaxJetPower {
		jetPower = MaxJetPower
	}

	return &Squid{
		Name:       name,
		Age:        age,
		Speed:      speed,
		JetPower:   jetPower,
		Position:   Position{X: 0, Y: 0, Z: 0},
		IsSwimming: false,
	}
}

// Swim 游泳方法 - 体现Go的高效和直接
func (s *Squid) Swim(destination Position, wg *sync.WaitGroup) {
	defer wg.Done()
	
	s.IsSwimming = true
	distance := s.calculateDistance(destination)
	timeNeeded := distance / s.Speed

	fmt.Printf("🚀 %s 正在以%.1fm/s速度游向目标\n", s.Name, s.Speed)
	fmt.Printf("   距离: %.1fm, 预计时间: %.1f秒\n", distance, timeNeeded)

	// 模拟游泳过程
	for i := 0; i < 3; i++ {
		time.Sleep(300 * time.Millisecond)
		fmt.Printf("   %s 喷水推进中...\n", s.Name)
	}

	s.Position = destination
	s.IsSwimming = false
	fmt.Printf("   ✅ %s 到达目的地 (%.1f, %.1f, %.1f)\n", 
		s.Name, destination.X, destination.Y, destination.Z)
}

// JetEscape 喷水逃跑 - 并发处理
func (s *Squid) JetEscape(predatorPosition Position, wg *sync.WaitGroup) {
	defer wg.Done()

	fmt.Printf("⚠️  %s 检测到危险！启动喷水逃跑机制\n", s.Name)
	
	// 计算逃跑方向（远离捕食者）
	escapeDirection := s.calculateEscapeDirection(predatorPosition)
	escapeSpeed := s.Speed * 1.5 // 逃跑时速度增加

	if escapeSpeed > MaxSpeed {
		escapeSpeed = MaxSpeed
	}

	// 使用channel进行通信 - Go的并发特性
	escapeCh := make(chan string, 2)

	go func() {
		time.Sleep(500 * time.Millisecond)
		escapeCh <- "释放墨汁迷惑敌人"
	}()

	go func() {
		time.Sleep(300 * time.Millisecond)
		escapeCh <- "高速喷水推进"
	}()

	// 接收并发结果
	for i := 0; i < 2; i++ {
		action := <-escapeCh
		fmt.Printf("   🎯 %s: %s\n", s.Name, action)
	}

	fmt.Printf("   🔥 %s 以%.1fm/s速度成功逃脱！方向: %.1f, %.1f, %.1f\n",
		s.Name, escapeSpeed, escapeDirection.X, escapeDirection.Y, escapeDirection.Z)
}

// 辅助方法
func (s *Squid) calculateDistance(target Position) float64 {
	dx := target.X - s.Position.X
	dy := target.Y - s.Position.Y
	dz := target.Z - s.Position.Z
	return dx*dx + dy*dy + dz*dz
}

func (s *Squid) calculateEscapeDirection(predator Position) Position {
	// 简单计算远离捕食者的方向
	return Position{
		X: s.Position.X - predator.X,
		Y: s.Position.Y - predator.Y, 
		Z: s.Position.Z - predator.Z,
	}
}

// JoinSchool 加入鱼群 - 体现群体行为
func (s *Squid) JoinSchool(school *School) {
	school.Lock()
	defer school.Unlock()
	
	s.School = school
	school.Members = append(school.Members, s)
	fmt.Printf("🐟 %s 加入了 %s 鱼群\n", s.Name, school.Name)
}

// SchoolSwim 鱼群游泳 - 并发同步
func (s *School) SchoolSwim(destination Position) {
	s.RLock()
	defer s.RUnlock()

	fmt.Printf("🐟🐟🐟 鱼群 %s 开始集体移动 (%d只鱿鱼)\n", s.Name, len(s.Members))

	var wg sync.WaitGroup
	for _, squid := range s.Members {
		wg.Add(1)
		go squid.Swim(destination, &wg)
	}

	wg.Wait()
	fmt.Printf("🎉 鱼群 %s 集体到达目的地\n", s.Name)
}

func main() {
	fmt.Println("=== Go 鱿鱼模拟器 ===\n")
	
	rand.Seed(time.Now().UnixNano())

	// 创建高效的鱿鱼 - Go的简洁语法
	squid1 := NewSquid("斯威夫特", 1, 8.5, 7.2)
	squid2 := NewSquid("喷气式", 2, 9.0, 6.8)
	squid3 := NewSquid("闪电", 1, 7.8, 7.5)

	fmt.Printf("创建鱿鱼: %s (速度: %.1fm/s)\n", squid1.Name, squid1.Speed)
	fmt.Printf("创建鱿鱼: %s (速度: %.1fm/s)\n", squid2.Name, squid2.Speed) 
	fmt.Printf("创建鱿鱼: %s (速度: %.1fm/s)\n", squid3.Name, squid3.Speed)
	fmt.Println()

	// 创建鱼群
	school := &School{Name: "快速突击队"}
	squid1.JoinSchool(school)
	squid2.JoinSchool(school)
	squid3.JoinSchool(school)
	fmt.Println()

	// 演示群体游泳
	destination := Position{X: 100, Y: 50, Z: -20}
	school.SchoolSwim(destination)
	fmt.Println()

	// 演示并发逃跑
	var wg sync.WaitGroup
	predator := Position{X: 110, Y: 60, Z: -15}

	fmt.Println("=== 遇到捕食者！紧急逃跑 ===")
	wg.Add(3)
	go squid1.JetEscape(predator, &wg)
	go squid2.JetEscape(predator, &wg) 
	go squid3.JetEscape(predator, &wg)
	wg.Wait()

	fmt.Println("\n✅ 所有鱿鱼安全逃脱！")
}