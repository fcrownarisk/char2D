Public Enum CreatureType
    Octopus
    CuttleFish
    Squid
End Enum

Public MustInherit Class Creature
    Public Property Id As Integer
    Public Property Name As String
    Public Property X As Double
    Public Property Y As Double
    Public Property Size As Double
    Public Property Energy As Double
    Public Property Type As CreatureType
    Public Property CurrentColor As Color
    Public Property BaseColor As Color
    Public Property IsCommunicating As Boolean
    Public Property IsCamouflaging As Boolean
    Public Property SignalRadius As Double
    Public Property SignalColor As Color
    Public Property SignalMessage As String
    Public Property AnimationPhase As Double
    Public Property TargetX As Double?
    Public Property TargetY As Double?
    Public Property Friends As List(Of Integer)
    Public Property Foes As List(Of Integer)
    Public Property LastSignalTime As Double
    
    Protected random As New Random()
    
    Public Sub New(id As Integer, name As String, x As Double, y As Double)
        Me.Id = id
        Me.Name = name
        Me.X = x
        Me.Y = y
        Me.Size = 15
        Me.Energy = 100
        Me.IsCommunicating = False
        Me.IsCamouflaging = False
        Me.SignalRadius = 100
        Me.AnimationPhase = 0
        Me.Friends = New List(Of Integer)()
        Me.Foes = New List(Of Integer)()
        Me.LastSignalTime = 0
    End Sub
    
    Public MustOverride Sub Update(neighbors As List(Of Creature), worldTime As Double)
    Public MustOverride Function GetSignalMessage() As String
    
    Public Overridable Sub Move()
        ' Random wandering behavior
        If Not TargetX.HasValue OrElse DistanceToTarget() < 10 Then
            ' Set new random target
            TargetX = X + random.Next(-100, 100)
            TargetY = Y + random.Next(-100, 100)
        End If
        
        ' Move toward target
        If TargetX.HasValue AndAlso TargetY.HasValue Then
            Dim dx As Double = TargetX.Value - X
            Dim dy As Double = TargetY.Value - Y
            Dim distance As Double = Math.Sqrt(dx * dx + dy * dy)
            
            If distance > 1 Then
                Dim speed As Double = 2.0
                X += (dx / distance) * speed
                Y += (dy / distance) * speed
            End If
        End If
        
        ' Animation
        AnimationPhase += 0.1
    End Sub
    
    Private Function DistanceToTarget() As Double
        If TargetX.HasValue AndAlso TargetY.HasValue Then
            Return Math.Sqrt(Math.Pow(TargetX.Value - X, 2) + Math.Pow(TargetY.Value - Y, 2))
        End If
        Return 0
    End Function
    
    Public Sub Communicate(message As String, color As Color, worldTime As Double)
        IsCommunicating = True
        SignalMessage = message
        SignalColor = color
        LastSignalTime = worldTime
    End Sub
    
    Public Sub ConsumeEnergy(amount As Double)
        Energy -= amount
        If Energy < 0 Then Energy = 0
    End Sub
    
    Public Function IsNearby(otherX As Double, otherY As Double, radius As Double) As Boolean
        Dim dx As Double = otherX - X
        Dim dy As Double = otherY - Y
        Return (dx * dx + dy * dy) <= (radius * radius)
    End Function
End Class

Public Class Octopus
    Inherits Creature
    
    Public Sub New(id As Integer, name As String, x As Double, y As Double)
        MyBase.New(id, name, x, y)
        Me.Type = CreatureType.Octopus
        Me.BaseColor = Color.FromArgb(180, 100, 80)  ' Reddish-brown
        Me.CurrentColor = BaseColor
        Me.Size = 20
    End Sub
    
    Public Overrides Sub Update(neighbors As List(Of Creature), worldTime As Double)
        ' Move
        Move()
        ConsumeEnergy(0.5)
        
        ' Check for food/energy
        For Each neighbor In neighbors
            If neighbor IsNot Me AndAlso IsNearby(neighbor.X, neighbor.Y, 30) Then
                ' Found another creature - could be friend or foe
                If Me.Friends.Contains(neighbor.Id) Then
                    ' Friend - share energy
                    If Me.Energy > 20 AndAlso neighbor.Energy < 80 Then
                        Me.Energy -= 5
                        neighbor.Energy += 5
                        Communicate($"Sharing with {neighbor.Name}", Color.Pink, worldTime)
                    End If
                Else
                    ' Unknown - get curious
                    CurrentColor = Color.FromArgb(200, 150, 100)  ' Brighter when curious
                End If
            End If
        Next
        
        ' Camouflage when not communicating
        If worldTime - LastSignalTime > 2 Then
            IsCommunicating = False
            IsCamouflaging = True
            ' Blend with background
            CurrentColor = Color.FromArgb(100, 70, 60)
        Else
            IsCamouflaging = False
        End If
        
        ' Occasionally communicate
        If random.NextDouble() < 0.02 Then
            Communicate("Octo-pattern: " & GetRandomPattern(), 
                       Color.FromArgb(200, 100, 50), worldTime)
        End If
    End Sub
    
    Private Function GetRandomPattern() As String
        Dim patterns() As String = {"camouflage", "warning", "courtship", "alarm"}
        Return patterns(random.Next(patterns.Length))
    End Function
    
    Public Overrides Function GetSignalMessage() As String
        Return $"Octopus {Name}: {SignalMessage}"
    End Function
End Class

Public Class CuttleFish
    Inherits Creature
    
    Public Property Pattern As String
    
    Public Sub New(id As Integer, name As String, x As Double, y As Double)
        MyBase.New(id, name, x, y)
        Me.Type = CreatureType.CuttleFish
        Me.BaseColor = Color.FromArgb(150, 180, 200)  ' Blue-green
        Me.CurrentColor = BaseColor
        Me.Pattern = "waves"
        Me.Size = 18
    End Sub
    
    Public Overrides Sub Update(neighbors As List(Of Creature), worldTime As Double)
        ' Move
        Move()
        ConsumeEnergy(0.6)
        
        ' Cuttlefish are masters of pattern communication
        Dim nearbyCount As Integer = 0
        
        For Each neighbor In neighbors
            If neighbor IsNot Me AndAlso IsNearby(neighbor.X, neighbor.Y, 50) Then
                nearbyCount += 1
                
                ' Change pattern based on neighbor type
                If neighbor.Type = CreatureType.Octopus Then
                    Pattern = "hypnotic waves"
                    CurrentColor = Color.FromArgb(200, 150, 200)  ' Purple for octopus interaction
                ElseIf neighbor.Type = CreatureType.Squid Then
                    Pattern = "pulsing dots"
                    CurrentColor = Color.FromArgb(100, 200, 200)  ' Cyan for squid interaction
                End If
            End If
        Next
        
        ' Communicate patterns
        If nearbyCount > 0 AndAlso random.NextDouble() < 0.05 Then
            Communicate($"Pattern: {Pattern} | Nearby: {nearbyCount}",
                       CurrentColor, worldTime)
        End If
        
        ' Reset communication status
        If worldTime - LastSignalTime > 1.5 Then
            IsCommunicating = False
        End If
    End Sub
    
    Public Overrides Function GetSignalMessage() As String
        Return $"Cuttlefish {Name}: {Pattern} pattern"
    End Function
End Class

Public Class Squid
    Inherits Creature
    
    Public Property Bioluminescence As Boolean
    Public Property FlashPattern As String
    
    Public Sub New(id As Integer, name As String, x As Double, y As Double)
        MyBase.New(id, name, x, y)
        Me.Type = CreatureType.Squid
        Me.BaseColor = Color.FromArgb(220, 150, 220)  ' Purple-pink
        Me.CurrentColor = BaseColor
        Me.Bioluminescence = False
        Me.FlashPattern = "steady"
        Me.Size = 16
    End Sub
    
    Public Overrides Sub Update(neighbors As List(Of Creature), worldTime As Double)
        ' Move (squid are faster)
        Move()
        ConsumeEnergy(0.8)  ' Squid use more energy
        
        ' Bioluminescent communication
        Dim flashNow As Boolean = False
        
        For Each neighbor In neighbors
            If neighbor IsNot Me AndAlso IsNearby(neighbor.X, neighbor.Y, 80) Then
                ' Squid communicate with light
                If neighbor.IsCommunicating Then
                    ' Respond to others' signals
                    flashNow = True
                    FlashPattern = "responding"
                    
                    ' Copy some of their signal color
                    SignalColor = Color.FromArgb(
                        255,
                        (CurrentColor.R + neighbor.SignalColor.R) \ 2,
                        (CurrentColor.G + neighbor.SignalColor.G) \ 2,
                        (CurrentColor.B + neighbor.SignalColor.B) \ 2
                    )
                End If
            End If
        Next
        
        ' Random bioluminescence
        If random.NextDouble() < 0.03 Then
            flashNow = True
            FlashPattern = GetRandomFlashPattern()
        End If
        
        If flashNow Then
            Bioluminescence = True
            CurrentColor = Color.White
            Communicate($"Flash: {FlashPattern}", Color.Cyan, worldTime)
        Else
            Bioluminescence = False
            CurrentColor = BaseColor
        End If
        
        ' Reset communication
        If worldTime - LastSignalTime > 1 Then
            IsCommunicating = False
        End If
    End Sub
    
    Private Function GetRandomFlashPattern() As String
        Dim patterns() As String = {"pulse", "strobe", "wave", "spiral", "alert"}
        Return patterns(random.Next(patterns.Length))
    End Function
    
    Public Overrides Function GetSignalMessage() As String
        Return $"Squid {Name}: {FlashPattern} bioluminescence"
    End Function
End Class