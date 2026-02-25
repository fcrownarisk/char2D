Public Class CommunicationLogForm
    Private world As OceanWorld
    Private WithEvents refreshTimer As New Timer()
    
    Public Sub New(oceanWorld As OceanWorld)
        InitializeComponent()
        Me.world = oceanWorld
        Me.Text = "Communication Log - Infinite Backroom"
        Me.Size = New Size(400, 600)
        Me.StartPosition = FormStartPosition.Manual
        Me.Location = New Point(1200, 100)
        
        SetupUI()
        
        refreshTimer.Interval = 1000
        refreshTimer.Start()
    End Sub
    
    Private Sub SetupUI()
        Dim listBox As New ListBox()
        listBox.Dock = DockStyle.Fill
        listBox.BackColor = Color.FromArgb(30, 30, 40)
        listBox.ForeColor = Color.Cyan
        listBox.Font = New Font("Consolas", 9)
        Me.Controls.Add(listBox)
        
        AddHandler refreshTimer.Tick, Sub()
            Dim events = world.GetCommunicationLog(50)
            listBox.Items.Clear()
            For Each ev In events
                listBox.Items.Add($"[{ev.Time:F1}s] Creature {ev.CreatureId}: {ev.Message}")
            Next
        End Sub
    End Sub
End Class