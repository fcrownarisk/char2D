package SQL

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type Squid struct {
	Name          string
	Age           int
	Speed         float64 
	JetPower      float64 
	School        *School
	Position      Position
	IsSwimming    bool
}

type Position struct {
	X, Y, Z float64
}

type School struct {
	Name     string
	Members  []*Squid
	sync.RWMutex
}

const (
	MaxSpeed    = 10.0
	MaxJetPower = 8.0
)

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

func (s *Squid) Swim(destination Position, wg *sync.WaitGroup) {
	defer wg.Done()
	
	s.IsSwimming = true
	distance := s.calculateDistance(destination)
	timeNeeded := distance / s.Speed

	fmt.Printf("🚀 %s \n", s.Name, s.Speed)
	fmt.Printf("   : %.1fm, : %.1f秒\n", distance, timeNeeded)

	for i := 0; i < 3; i++ {
		time.Sleep(300 * time.Millisecond)
		fmt.Printf("   %s ...\n", s.Name)
	}

	s.Position = destination
	s.IsSwimming = false
	fmt.Printf("   ✅ %s (%.1f, %.1f, %.1f)\n", 
		s.Name, destination.X, destination.Y, destination.Z)
}

// JetEscape 喷水逃跑 - 并发处理
func (s *Squid) JetEscape(predatorPosition Position, wg *sync.WaitGroup) {
	defer wg.Done()

	fmt.Printf("⚠️  %s \n", s.Name)

	escapeDirection := s.calculateEscapeDirection(predatorPosition)
	escapeSpeed := s.Speed * 1.5 

	if escapeSpeed > MaxSpeed {
		escapeSpeed = MaxSpeed
	}

	escapeCh := make(chan string, 2)

	go func() {
		time.Sleep(500 * time.Millisecond)
		escapeCh <- "释放墨汁迷惑敌人"
	}()

	go func() {
		time.Sleep(300 * time.Millisecond)
		escapeCh <- "高速喷水推进"
	}()

	for i := 0; i < 2; i++ {
		action := <-escapeCh
		fmt.Printf("   🎯 %s: %s\n", s.Name, action)
	}

	fmt.Printf("   🔥 %s 以%.1fm/s速度成功逃脱！方向: %.1f, %.1f, %.1f\n",
		s.Name, escapeSpeed, escapeDirection.X, escapeDirection.Y, escapeDirection.Z)
}

func (s *Squid) calculateDistance(target Position) float64 {
	dx := target.X - s.Position.X
	dy := target.Y - s.Position.Y
	dz := target.Z - s.Position.Z
	return dx*dx + dy*dy + dz*dz
}

func (s *Squid) calculateEscapeDirection(predator Position) Position {

	return Position{
		X: s.Position.X - predator.X,
		Y: s.Position.Y - predator.Y, 
		Z: s.Position.Z - predator.Z,
	}
}

func (s *Squid) JoinSchool(school *School) {
	school.Lock()
	defer school.Unlock()
	
	s.School = school
	school.Members = append(school.Members, s)
	fmt.Printf("🐟 %s 加入了 %s 鱼群\n", s.Name, school.Name)
}

func (s *School) SchoolSwim(destination Position) {
	s.RLock()
	defer s.RUnlock()

	fmt.Printf("🐟 %s \n", s.Name, len(s.Members))

	var wg sync.WaitGroup
	for _, squid := range s.Members {
		wg.Add(1)
		go squid.Swim(destination, &wg)
	}

	wg.Wait()
	fmt.Printf("🎉 \n", s.Name)
}

func main() {
	fmt.Println("Go\n")
	
	rand.Seed(time.Now().UnixNano())

	squid1 := NewSquid("Swift", 1, 8.5, 7.2)
	squid2 := NewSquid("jetpack", 2, 9.0, 6.8)
	squid3 := NewSquid("lightning", 1, 7.8, 7.5)

	fmt.Printf(": %s (speed: %.1fm/s)\n", squid1.Name, squid1.Speed)
	fmt.Println()
	school := &School{Name: ""}
	squid1.JoinSchool(school)
	fmt.Println()

	destination := Position{X: 100, Y: 50, Z: -20}
	school.SchoolSwim(destination)
	fmt.Println()

	var wg sync.WaitGroup
	predator := Position{X: 110, Y: 60, Z: -15}

	fmt.Println("")
	wg.Add(3)
	go squid1.JetEscape(predator, &wg)

	wg.Wait()
}
