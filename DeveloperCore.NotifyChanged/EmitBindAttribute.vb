<AttributeUsage(AttributeTargets.Field, AllowMultiple := True)>
Public Class EmitBindAttribute
    Inherits Attribute

    Public Sub New(source As String, Optional [property] As String = Nothing)
        Me.Source = source
        Me.[Property] = [property]
    End Sub

    Public Property Source As String
    Public Property [Property] As String
End Class