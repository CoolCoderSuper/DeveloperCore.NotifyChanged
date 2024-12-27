<AttributeUsage(AttributeTargets.Field, AllowMultiple := True)>
Public Class EmitConditionAttribute
    Inherits Attribute
    Public Sub New(name As String)
        MethodName = name
    End Sub
    
    Public Property MethodName As String
End Class
