Public Class OceanWorld
    Private creatures As New List(Of Creature)()
    Private worldTime As Double = 0
    Private width As Integer
    Private height As Integer
    Private communicationHistory As New List(Of CommunicationEvent)()
    
    Public ReadOnly Property CreatureCount As Integer
        Get
            Return creatures.Count
        End Get
    End Property
    
    Public ReadOnly Property WorldTime As Double
        Get
            Return worldTime
        End Get
    End Property
    
    Public Sub New(width As Integer, height As Integer)
        Me.width = width
        Me.height = height
    End Sub
    
    Public Sub AddCreature(creature As Creature)
        creatures.Add(creature)
    End Sub
    
    Public Sub RemoveCreature(id As Integer)
        creatures.RemoveAll(Function(c) c.Id = id)
    End Sub
    
    Public Sub Clear()
        creatures.Clear()
    End Sub
    
    Public Function GetAllCreatures() As List(Of Creature)
        Return creatures
    End Function
    
    Public Sub Update()
        worldTime += 0.05
        
        ' Update each creature with knowledge of neighbors
        For Each creature In creatures.ToList()  ' ToList to avoid modification issues
            Dim neighbors As New List(Of Creature)(creatures)
            creature.Update(neighbors, worldTime)
            
            ' Check boundaries (infinite world wrapping)
            If creature.X < 0 Then creature.X = width
            If creature.X > width Then creature.X = 0
            If creature.Y < 0 Then creature.Y = height
            If creature.Y > height Then creature.Y = 0
            
            ' Record communications
            If creature.IsCommunicating Then
                communicationHistory.Add(New CommunicationEvent(
                    worldTime, creature.Id, creature.SignalMessage))
            End If
            
            ' Remove dead creatures (energy = 0)
            If creature.Energy <= 0 Then
                creatures.Remove(creature)
            End If
        Next
        
        ' Limit history size
        If communicationHistory.Count > 1000 Then
            communicationHistory.RemoveRange(0, 500)
        End If
    End Sub
    
    Public Function FindNearestCreature(x As Integer, y As Integer, maxDistance As Integer) As Creature
        Dim nearest As Creature = Nothing
        Dim minDistance As Double = Double.MaxValue
        
        For Each creature In creatures
            Dim distance As Double = Math.Sqrt(Math.Pow(creature.X - x, 2) + Math.Pow(creature.Y - y, 2))
            If distance < minDistance AndAlso distance <= maxDistance Then
                minDistance = distance
                nearest = creature
            End If
        Next
        
        Return nearest
    End Function
    
    Public Function GetNextId() As Integer
        If creatures.Count = 0 Then Return 1
        Return creatures.Max(Function(c) c.Id) + 1
    End Function
    
    Public Function GetCommunicationLog(count As Integer) As List(Of CommunicationEvent)
        Return communicationHistory.OrderByDescending(Function(e) e.Time).Take(count).ToList()
    End Function
End Class

Public Class CommunicationEvent
    Public Property Time As Double
    Public Property CreatureId As Integer
    Public Property Message As String
    
    Public Sub New(time As Double, creatureId As Integer, message As String)
        Me.Time = time
        Me.CreatureId = creatureId
        Me.Message = message
    End Sub
End Class