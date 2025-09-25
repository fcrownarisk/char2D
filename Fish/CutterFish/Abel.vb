Namespace Abel
    Public Interface IRing(Of T)
        Inherits IGroup(Of T) ' Additive group
        Function Multiply(a As T, b As T) As T
        Function MultiplicativeIdentity() As T
    End Interface

    Public Module Ring
        Public Function MultiplyNTimes(Of T)(ring As IRing(Of T), element As T, n As Integer) As T
            If n = 0 Then Return ring.MultiplicativeIdentity()
            Dim result As T = element
            For i = 2 To n
                result = ring.Multiply(result, element)
            Next
            Return result
        End Function
    End Module
End Namespace

Namespace Abel
    Public Interface IField(Of T)
        Inherits IRing(Of T)
        Function MultiplicativeInverse(a As T) As T
        Function IsZero(a As T) As Boolean
    End Interface

    Public Module Field
        Public Function Divide(Of T)(field As IField(Of T), a As T, b As T) As T
            If field.IsZero(b) Then Throw New DivideByZeroException("Cannot divide by zero.")
            Return field.Multiply(a, field.MultiplicativeInverse(b))
        End Function
    End Module
End Namespace

Public Class RationalNumber
    Implements IField(Of RationalNumber)

    Public Property Numerator As Integer
    Public Property Denominator As Integer

    Public Sub New(num As Integer, den As Integer)
        Numerator = num
        Denominator = If(den <> 0, den, 1)
        Simplify()
    End Sub

    Private Sub Simplify()
        Dim gcdValue As Integer = Gcd(Math.Abs(Numerator), Math.Abs(Denominator))
        Numerator \= gcdValue
        Denominator \= gcdValue
        If Denominator < 0 Then
            Numerator = -Numerator
            Denominator = -Denominator
        End If
    End Sub

    Private Function Gcd(a As Integer, b As Integer) As Integer
        Return If(b = 0, a, Gcd(b, a Mod b))
    End Function

    Public Function Combine(other As RationalNumber) As RationalNumber Implements IGroup(Of RationalNumber).Combine
        Return New RationalNumber(
            Numerator * other.Denominator + other.Numerator * Denominator,
            Denominator * other.Denominator)
    End Function

    Public Function Identity() As RationalNumber Implements IGroup(Of RationalNumber).Identity
        Return New RationalNumber(0, 1)
    End Function

    Public Function Inverse() As RationalNumber Implements IGroup(Of RationalNumber).Inverse
        Return New RationalNumber(-Numerator, Denominator)
    End Function

    Public Function Multiply(other As RationalNumber) As RationalNumber Implements IRing(Of RationalNumber).Multiply
        Return New RationalNumber(Numerator * other.Numerator, Denominator * other.Denominator)
    End Function

    Public Function MultiplicativeIdentity() As RationalNumber Implements IRing(Of RationalNumber).MultiplicativeIdentity
        Return New RationalNumber(1, 1)
    End Function

    Public Function MultiplicativeInverse() As RationalNumber Implements IField(Of RationalNumber).MultiplicativeInverse
        If Numerator = 0 Then Throw New DivideByZeroException("Zero has no multiplicative inverse.")
        Return New RationalNumber(Denominator, Numerator)
    End Function

    Public Function IsZero() As Boolean Implements IField(Of RationalNumber).IsZero
        Return Numerator = 0
    End Function

    Public Function IsAbelian() As Boolean Implements IGroup(Of RationalNumber).IsAbelian
        Return True
    End Function
End Class