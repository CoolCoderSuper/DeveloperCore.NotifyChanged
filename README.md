# DeveloperCore.NotifyChanged

Automatically generate getters and setters for properties in VB.

Here are some examples.

```vb
Public Class Customer
    Inherits BindableObject

    Private _customer As BackingCustomer
    
    <NotifyChanged>
    <EmitBind(NameOf(_customer), NameOf(BackingCustomer.Name))>
    <EmitCall(NameOf(Test1))>
    <EmitCall(NameOf(Test2))>
    Private _name As String
    <NotifyChanged>
    <EmitBind(NameOf(_customer), NameOf(BackingCustomer.Age))>
    <EmitCondition(NameOf(Cond1))>
    <EmitCondition(NameOf(Cond2))>
    Private _age As Integer
    
    <NotifyChanged>
    Private m_strAddress As String
    
    Private Function BeforeSetName(value As String, oldValue As String) As Boolean
        Return _name.Length > 0
    End Function

    Private Function BeforeSetAge(value As Integer, oldValue As Integer) As Boolean
        Return _name.Length > 0
    End Function
    
    Private Sub SetAge(value As Integer)
        Console.WriteLine(value)
    End Sub
    
    Private Sub Test1()
        
    End Sub
    
    Private Sub Test2()
        
    End Sub
    
    Private Function Cond1() As Boolean
        Return True
    End Function
    
    Private Function Cond2() As Boolean
        Return True
    End Function 
End Class

Public Class BackingCustomer
    Public Property Name As String
    Public Property Age As Integer
End Class
```

Generates this: 
```vb
Public Property Age As Int32
    Get
        Return _age
    End Get
    Set
        If BeforeSetAge(Value, _age) AndAlso Cond1 AndAlso Cond2 AndAlso RaiseAndSetIfChanged(_age, Value)
            _customer.Age = Value
            SetAge(Value)
        End If
    End Set
End Property

Public Property Name As String
    Get
        Return _name
    End Get
    Set
        If BeforeSetName(Value, _name) AndAlso RaiseAndSetIfChanged(_name, Value)
            _customer.Name = Value
            Test1
            Test2
        End If
    End Set
End Property

Public Property Address As String
    Get
        Return m_strAddress
    End Get
    Set
        If RaiseAndSetIfChanged(m_strAddress, Value)
        End If
    End Set
End Property
```

BindableObject:
```vb
Public MustInherit Class BindableObject
    Implements INotifyPropertyChanged, INotifyPropertyChanging

    Public Function RaiseAndSetIfChanged(Of T)(ByRef field As T, value As T, <CallerMemberName> Optional propertyName As String = "") As Boolean
        If Not EqualityComparer(Of T).Default.Equals(field, value) Then
            RaiseNotifyPropertyChanging(propertyName)
            field = value
            RaiseNotifyPropertyChanged(propertyName)
            Return True
        End If
        Return False
    End Function

    Public Sub RaiseNotifyPropertyChanging(propertyName As String)
        RaiseEvent PropertyChanging(Me, New PropertyChangingEventArgs(propertyName))
    End Sub

    Public Sub RaiseNotifyPropertyChanged(propertyName As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

    Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged
    Public Event PropertyChanging As PropertyChangingEventHandler Implements INotifyPropertyChanging.PropertyChanging
End Class
```
