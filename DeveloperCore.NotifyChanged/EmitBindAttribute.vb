<AttributeUsage(AttributeTargets.Field)>
Public Class EmitBindAttribute
    Inherits Attribute

    Public Sub New(source As String, [property] As String)
        Me.Source = source
        Me.[Property] = [property]
    End Sub

    Public Property Source As String
    Public Property [Property] As String
End Class