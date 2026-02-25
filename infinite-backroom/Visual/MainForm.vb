
Public Class Form
    Private WithEvents timer As New Timer()
    Private oceanWorld As OceanWorld
    Private lastMousePos As Point
    Private selectedCreature As Creature
    Private showDebugInfo As Boolean = True
    
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.DoubleBuffered = True
        Me.Text = "Infinite Backroom: Octopus • Cuttlefish • Squid"
        Me.Size = New Size(1200, 800)
        Me.BackColor = Color.FromArgb(0, 20, 40)
        
        ' Initialize ocean world
        oceanWorld = New OceanWorld(Me.ClientSize.Width, Me.ClientSize.Height)
        
        ' Setup timer for animation
        timer.Interval = 50 ' 20 FPS
        timer.Start()
        
        ' Add initial creatures
        AddInitialCreatures()
    End Sub
    
    Private Sub AddInitialCreatures()
        Dim rand As New Random()
        
        ' Add Octopuses (rock artists)
        For i = 1 To 3
            oceanWorld.AddCreature(New Octopus(
                id:=i,
                name:="Octo-" & i,
                x:=rand.Next(100, Me.ClientSize.Width - 100),
                y:=rand.Next(100, Me.ClientSize.Height - 100)
            ))
        Next
        
        ' Add Cuttlefish (pattern masters)
        For i = 1 To 4
            oceanWorld.AddCreature(New CuttleFish(
                id:=i + 10,
                name:="Cuttle-" & i,
                x:=rand.Next(100, Me.ClientSize.Width - 100),
                y:=rand.Next(100, Me.ClientSize.Height - 100)
            ))
        Next
        
        ' Add Squid (bioluminescent communicators)
        For i = 1 To 4
            oceanWorld.AddCreature(New Squid(
                id:=i + 20,
                name:="Squidy-" & i,
                x:=rand.Next(100, Me.ClientSize.Width - 100),
                y:=rand.Next(100, Me.ClientSize.Height - 100)
            ))
        Next
    End Sub
    
    Private Sub Timer_Tick(sender As Object, e As EventArgs) Handles timer.Tick
        ' Update world simulation
        oceanWorld.Update()
        
        ' Redraw
        Me.Invalidate()
    End Sub
    
    Private Sub Form1_Paint(sender As Object, e As PaintEventArgs) Handles MyBase.Paint
        Dim g As Graphics = e.Graphics
        g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
        
        ' Draw ocean background
        DrawOcean(g)
        
        ' Draw all creatures
        For Each creature In oceanWorld.GetAllCreatures()
            DrawCreature(g, creature)
            
            ' Draw communication signals if active
            If creature.IsCommunicating Then
                DrawCommunicationSignal(g, creature)
            End If
            
            ' Draw name if selected
            If selectedCreature IsNot Nothing AndAlso selectedCreature.Id = creature.Id Then
                DrawCreatureInfo(g, creature)
            End If
        Next
        
        ' Draw debug info
        If showDebugInfo Then
            DrawDebugInfo(g)
        End If
        
        ' Draw boundaries indicator
        DrawWorldBounds(g)
    End Sub
    
    Private Sub DrawOcean(g As Graphics)
        ' Gradient ocean background
        Dim oceanGradient As New Drawing2D.LinearGradientBrush(
            Me.ClientRectangle,
            Color.FromArgb(0, 30, 60),
            Color.FromArgb(0, 10, 30),
            Drawing2D.LinearGradientMode.Vertical)
        g.FillRectangle(oceanGradient, Me.ClientRectangle)
        
        ' Draw light rays from surface
        For i = 0 To 5
            Dim alpha As Integer = 20 - i * 3
            Using pen As New Pen(Color.FromArgb(alpha, 100, 200, 255), 1)
                Dim x1 As Integer = i * 200
                Dim y1 As Integer = 0
                Dim x2 As Integer = (i + 1) * 200 + 100
                Dim y2 As Integer = Me.ClientSize.Height
                g.DrawLine(pen, x1, y1, x2, y2)
            End Using
        Next
    End Sub
    
    Private Sub DrawCreature(g As Graphics, creature As Creature)
        Dim creatureRect As New Rectangle(
            CInt(creature.X - creature.Size),
            CInt(creature.Y - creature.Size),
            CInt(creature.Size * 2),
            CInt(creature.Size * 2)
        )
        
        ' Draw creature based on its type and current state
        Select Case creature.Type
            Case CreatureType.Octopus
                DrawOctopus(g, creature)
            Case CreatureType.CuttleFish
                DrawCuttleFish(g, creature)
            Case CreatureType.Squid
                DrawSquid(g, creature)
        End Select
        
        ' Draw energy bar
        DrawEnergyBar(g, creature)
    End Sub
    
    Private Sub DrawOctopus(g As Graphics, octopus As Octopus)
        ' Body
        Using bodyBrush As New SolidBrush(octopus.CurrentColor)
            g.FillEllipse(bodyBrush, CInt(octopus.X - octopus.Size), CInt(octopus.Y - octopus.Size),
                         CInt(octopus.Size * 2), CInt(octopus.Size * 1.5))
        End Using
        
        ' Eyes (expressive)
        DrawEyes(g, octopus)
        
        ' Tentacles (8 for octopus)
        For i = 0 To 7
            Dim angle As Double = i * Math.PI / 4 + octopus.AnimationPhase
            Dim tentacleLength As Integer = CInt(octopus.Size * 1.2)
            Dim endX As Integer = CInt(octopus.X + Math.Cos(angle) * tentacleLength)
            Dim endY As Integer = CInt(octopus.Y + Math.Sin(angle) * tentacleLength)
            
            Using tentaclePen As New Pen(octopus.CurrentColor, 3)
                g.DrawLine(tentaclePen, CInt(octopus.X), CInt(octopus.Y), endX, endY)
            End Using
        Next
        
        ' Texture (camouflage pattern if active)
        If octopus.IsCamouflaging Then
            For i = 0 To 5
                Dim spotX As Integer = CInt(octopus.X + (i - 2.5) * 5)
                Dim spotY As Integer = CInt(octopus.Y + (i Mod 3 - 1) * 5)
                Using spotBrush As New SolidBrush(Color.FromArgb(100, 0, 0, 0))
                    g.FillEllipse(spotBrush, spotX - 2, spotY - 2, 4, 4)
                End Using
            Next
        End If
    End Sub
    
    Private Sub DrawCuttleFish(g As Graphics, cuttle As CuttleFish)
        ' Main body (cuttlebone shape)
        Dim points As PointF() = {
            New PointF(cuttle.X - cuttle.Size * 0.8, cuttle.Y),
            New PointF(cuttle.X, cuttle.Y - cuttle.Size),
            New PointF(cuttle.X + cuttle.Size * 0.8, cuttle.Y),
            New PointF(cuttle.X, cuttle.Y + cuttle.Size * 0.8)
        }
        
        Using bodyBrush As New SolidBrush(cuttle.CurrentColor)
            g.FillPolygon(bodyBrush, points)
        End Using
        
        ' Waving fin
        Dim finOffset As Integer = CInt(Math.Sin(cuttle.AnimationPhase) * 5)
        Using finPen As New Pen(Color.FromArgb(150, cuttle.CurrentColor.R, 
                                                cuttle.CurrentColor.G, 
                                                cuttle.CurrentColor.B), 2)
            g.DrawArc(finPen,
                     CInt(cuttle.X - cuttle.Size),
                     CInt(cuttle.Y - cuttle.Size * 0.3 + finOffset),
                     CInt(cuttle.Size * 2),
                     CInt(cuttle.Size * 0.6), 0, 180)
        End Using
        
        ' Eyes
        DrawEyes(g, cuttle)
        
        ' W-shaped pupil (cuttlefish distinctive)
        Using pupilBrush As New SolidBrush(Color.Black)
            g.FillRectangle(pupilBrush, CInt(cuttle.X - 3), CInt(cuttle.Y - 4), 2, 8)
            g.FillRectangle(pupilBrush, CInt(cuttle.X + 1), CInt(cuttle.Y - 4), 2, 8)
        End Using
    End Sub
    
    Private Sub DrawSquid(g As Graphics, squid As Squid)
        ' Torpedo body
        Using bodyBrush As New SolidBrush(squid.CurrentColor)
            Dim bodyPath As New Drawing2D.GraphicsPath()
            bodyPath.AddEllipse(CInt(squid.X - squid.Size * 0.7),
                                CInt(squid.Y - squid.Size * 0.5),
                                CInt(squid.Size * 1.4),
                                CInt(squid.Size))
            g.FillPath(bodyBrush, bodyPath)
        End Using
        
        ' Head/eyes
        DrawEyes(g, squid)
        
        ' Tentacles (10 for squid - 8 arms + 2 longer tentacles)
        For i = 0 To 7
            Dim angle As Double = i * Math.PI / 4 + 0.2
            Dim tentacleLength As Integer = CInt(squid.Size * 1.5)
            Dim endX As Integer = CInt(squid.X + Math.Cos(angle) * tentacleLength)
            Dim endY As Integer = CInt(squid.Y + Math.Sin(angle) * tentacleLength)
            
            Using tentaclePen As New Pen(squid.CurrentColor, 2)
                g.DrawLine(tentaclePen, CInt(squid.X + 5), CInt(squid.Y), endX, endY)
            End Using
        Next
        
        ' Two longer hunting tentacles
        For i = -1 To 1 Step 2
            Dim endX As Integer = CInt(squid.X + i * squid.Size * 2.5)
            Dim endY As Integer = CInt(squid.Y - 5)
            Using tentaclePen As New Pen(squid.CurrentColor, 3)
                g.DrawLine(tentaclePen, CInt(squid.X), CInt(squid.Y), endX, endY)
            End Using
        Next
        
        ' Bioluminescence if communicating
        If squid.IsCommunicating Then
            Using glowBrush As New SolidBrush(Color.FromArgb(100, squid.SignalColor))
                g.FillEllipse(glowBrush,
                             CInt(squid.X - squid.Size * 2),
                             CInt(squid.Y - squid.Size * 2),
                             CInt(squid.Size * 4),
                             CInt(squid.Size * 4))
            End Using
        End If
    End Sub
    
    Private Sub DrawEyes(g As Graphics, creature As Creature)
        ' Eye whites
        Using whiteBrush As New SolidBrush(Color.White)
            g.FillEllipse(whiteBrush, CInt(creature.X - 6), CInt(creature.Y - 8), 6, 8)
            g.FillEllipse(whiteBrush, CInt(creature.X), CInt(creature.Y - 8), 6, 8)
        End Using
        
        ' Pupils (follow mouse if selected)
        Dim pupilOffsetX As Integer = 0
        Dim pupilOffsetY As Integer = 0
        
        If selectedCreature IsNot Nothing AndAlso selectedCreature.Id = creature.Id Then
            pupilOffsetX = Math.Sign(lastMousePos.X - creature.X) * 2
            pupilOffsetY = Math.Sign(lastMousePos.Y - creature.Y) * 2
        End If
        
        Using pupilBrush As New SolidBrush(Color.Black)
            g.FillEllipse(pupilBrush,
                         CInt(creature.X - 4 + pupilOffsetX),
                         CInt(creature.Y - 6 + pupilOffsetY), 3, 4)
            g.FillEllipse(pupilBrush,
                         CInt(creature.X + 2 + pupilOffsetX),
                         CInt(creature.Y - 6 + pupilOffsetY), 3, 4)
        End Using
    End Sub
    
    Private Sub DrawCommunicationSignal(g As Graphics, creature As Creature)
        ' Draw signal ripples
        Dim pulseSize As Integer = CInt(creature.SignalRadius * (0.5 + 0.5 * Math.Sin(creature.AnimationPhase * 2)))
        
        For i = 0 To 2
            Dim alpha As Integer = 100 - i * 30
            Using ripplePen As New Pen(Color.FromArgb(alpha, creature.SignalColor), 2)
                ripplePen.DashStyle = Drawing2D.DashStyle.Dot
                g.DrawEllipse(ripplePen,
                             CInt(creature.X - pulseSize * (i + 1)),
                             CInt(creature.Y - pulseSize * (i + 1)),
                             CInt(pulseSize * 2 * (i + 1)),
                             CInt(pulseSize * 2 * (i + 1)))
            End Using
        Next
        
        ' Draw signal type indicator
        Using font As New Font("Segoe UI", 8)
            TextRenderer.DrawText(g, creature.SignalMessage, font,
                                  New Point(CInt(creature.X - 30), CInt(creature.Y - 40)),
                                  creature.SignalColor)
        End Using
    End Sub
    
    Private Sub DrawEnergyBar(g As Graphics, creature As Creature)
        Dim barWidth As Integer = 40
        Dim barHeight As Integer = 4
        Dim barX As Integer = CInt(creature.X - barWidth / 2)
        Dim barY As Integer = CInt(creature.Y - creature.Size - 10)
        
        ' Background
        Using backBrush As New SolidBrush(Color.FromArgb(100, 50, 50, 50))
            g.FillRectangle(backBrush, barX, barY, barWidth, barHeight)
        End Using
        
        ' Energy
        Dim energyWidth As Integer = CInt(barWidth * creature.Energy / 100)
        Dim energyColor As Color = Color.FromArgb(
            255 - CInt(creature.Energy * 2.55),
            CInt(creature.Energy * 2.55),
            0)
        
        Using energyBrush As New SolidBrush(energyColor)
            g.FillRectangle(energyBrush, barX, barY, energyWidth, barHeight)
        End Using
    End Sub
    
    Private Sub DrawCreatureInfo(g As Graphics, creature As Creature)
        Dim infoY As Integer = CInt(creature.Y - creature.Size - 30)
        
        Using bgBrush As New SolidBrush(Color.FromArgb(150, 0, 0, 0))
            g.FillRectangle(bgBrush, CInt(creature.X - 60), infoY - 15, 120, 40)
        End Using
        
        Using font As New Font("Segoe UI", 8, FontStyle.Bold)
            TextRenderer.DrawText(g, creature.Name & " (" & creature.Type.ToString() & ")", font,
                                  New Point(CInt(creature.X - 55), infoY - 10),
                                  Color.Cyan)
            TextRenderer.DrawText(g, $"Signal: {creature.SignalMessage}", font,
                                  New Point(CInt(creature.X - 55), infoY),
                                  Color.LightGreen)
        End Using
    End Sub
    
    Private Sub DrawDebugInfo(g As Graphics)
        Dim info As String = $"Creatures: {oceanWorld.CreatureCount} | Time: {oceanWorld.WorldTime:F1}s | " &
                             $"Selected: {If(selectedCreature IsNot Nothing, selectedCreature.Name, "None")}"
        
        Using bgBrush As New SolidBrush(Color.FromArgb(150, 0, 0, 0))
            g.FillRectangle(bgBrush, 10, 10, 400, 20)
        End Using
        
        Using font As New Font("Segoe UI", 9)
            TextRenderer.DrawText(g, info, font, New Point(15, 12), Color.White)
        End Using
    End Sub
    
    Private Sub DrawWorldBounds(g As Graphics)
        ' Draw infinite world indicator
        Using pen As New Pen(Color.FromArgb(50, Color.Cyan), 1)
            pen.DashStyle = Drawing2D.DashStyle.Dash
            g.DrawRectangle(pen, 50, 50, Me.ClientSize.Width - 100, Me.ClientSize.Height - 100)
        End Using
        
        ' "Infinite Backroom" text
        Using font As New Font("Segoe UI", 16, FontStyle.Bold)
            TextRenderer.DrawText(g, "∞ INFINITE BACKROOM ∞", font,
                                  New Point(Me.ClientSize.Width / 2 - 200, 20),
                                  Color.FromArgb(100, Color.Cyan))
        End Using
    End Sub
    
    Private Sub Form1_MouseMove(sender As Object, e As MouseEventArgs) Handles MyBase.MouseMove
        lastMousePos = e.Location
    End Sub
    
    Private Sub Form1_MouseClick(sender As Object, e As MouseEventArgs) Handles MyBase.MouseClick
        ' Select creature under mouse
        selectedCreature = oceanWorld.FindNearestCreature(e.X, e.Y, 20)
    End Sub
    
    Private Sub Form1_KeyDown(sender As Object, e As KeyEventArgs) Handles MyBase.KeyDown
        Select Case e.KeyCode
            Case Keys.Space
                ' Add new random creature
                Dim rand As New Random()
                Dim creatureType As Integer = rand.Next(0, 3)
                Dim newId As Integer = oceanWorld.GetNextId()
                
                Select Case creatureType
                    Case 0
                        oceanWorld.AddCreature(New Octopus(newId, "Octo-" & newId,
                                                          rand.Next(100, Me.ClientSize.Width - 100),
                                                          rand.Next(100, Me.ClientSize.Height - 100)))
                    Case 1
                        oceanWorld.AddCreature(New CuttleFish(newId, "Cuttle-" & newId,
                                                             rand.Next(100, Me.ClientSize.Width - 100),
                                                             rand.Next(100, Me.ClientSize.Height - 100)))
                    Case 2
                        oceanWorld.AddCreature(New Squid(newId, "Squidy-" & newId,
                                                        rand.Next(100, Me.ClientSize.Width - 100),
                                                        rand.Next(100, Me.ClientSize.Height - 100)))
                End Select
                
            Case Keys.D
                showDebugInfo = Not showDebugInfo
                
            Case Keys.C
                ' Clear all creatures
                oceanWorld.Clear()
                AddInitialCreatures()
        End Select
    End Sub
End Class