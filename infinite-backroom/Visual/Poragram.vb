Module Program
    <STAThread>
    Sub Main()
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        
        Dim mainForm As New Form1()
        
        ' Optional: Show communication log
        AddHandler mainForm.Shown, Sub()
            Dim logForm As New CommunicationLogForm(mainForm.oceanWorld)
            logForm.Show()
        End Sub
        
        Application.Run(mainForm)
    End Sub
End Module