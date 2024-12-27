Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<Generator(LanguageNames.VisualBasic)>
Public Class NotifyChangedGenerator
    Implements IIncrementalGenerator

    Public Sub Initialize(initContext As IncrementalGeneratorInitializationContext) Implements IIncrementalGenerator.Initialize
        Dim values = initContext.SyntaxProvider.ForAttributeWithMetadataName("DeveloperCore.NotifyChanged.NotifyChangedAttribute", Function(node, token) True, AddressOf ProcessNotifyChangedAttribute)
        initContext.RegisterSourceOutput(values, AddressOf Generate)
    End Sub

    Private Sub Generate(context As SourceProductionContext, info As PropertyGenInfo)
        context.AddSource(info.Name & ".g.vb", info.Syntax.ToFullString())
    End Sub

    Private Function ProcessNotifyChangedAttribute(context As GeneratorAttributeSyntaxContext, token As CancellationToken) As PropertyGenInfo
        Dim fieldSymbol As IFieldSymbol = context.TargetSymbol
        Dim attr = context.Attributes.First(Function(x) x.AttributeClass.Name = "NotifyChangedAttribute")
        Dim providedName As String = attr.NamedArguments.FirstOrDefault(Function(x) x.Key = "Name").Value.Value
        Dim actualName = If(providedName, GetPropertyName(context.TargetSymbol.Name))
        Dim hasSetterMeth = context.TargetSymbol.ContainingType.GetMembers().Any(Function(x) x.Name = $"Set{actualName}")
        Dim hasSetterPreCondMeth = context.TargetSymbol.ContainingType.GetMembers().Any(Function(x) x.Name = $"BeforeSet{actualName}")
        Dim methodsToCall = context.TargetSymbol.GetAttributes().Where(Function(x) x.AttributeClass.Name = "EmitCallAttribute").Select(Function(x) CStr(x.ConstructorArguments.First().Value)).ToArray()
        Dim conditionalMethods = context.TargetSymbol.GetAttributes().Where(Function(x) x.AttributeClass.Name = "EmitConditionAttribute").Select(Function(x) CStr(x.ConstructorArguments.First().Value)).ToArray()
        Dim hasAnyPreCond = conditionalMethods.Any() OrElse hasSetterPreCondMeth
        Dim propertyGetter =
                SyntaxFactory.GetAccessorBlock(SyntaxFactory.GetAccessorStatement()).
                    AddStatements(SyntaxFactory.ReturnStatement(SyntaxFactory.ParseName(context.TargetSymbol.Name)))
        Dim valueSetStatement As StatementSyntax = SyntaxFactory.ExpressionStatement(
            SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName("RaiseAndSetIfChanged")).
                AddArgumentListArguments({
                    SyntaxFactory.SimpleArgument(SyntaxFactory.IdentifierName(context.TargetSymbol.Name)),
                    SyntaxFactory.SimpleArgument(SyntaxFactory.IdentifierName("Value"))
                })
        )
        Dim methodsToCallInvocations As New List(Of StatementSyntax)
        If hasSetterMeth Then
            methodsToCallInvocations.Add(
                    SyntaxFactory.ExpressionStatement(
                        SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName($"Set{actualName}")).
                            AddArgumentListArguments({SyntaxFactory.SimpleArgument(SyntaxFactory.IdentifierName("Value"))})
                    )
            )
        End If
        For Each method In methodsToCall
            methodsToCallInvocations.Add(
                    SyntaxFactory.ExpressionStatement(
                        SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName(method))
                    )
            )
        Next
        If hasAnyPreCond Then
            Dim conditionalExpr As ExpressionSyntax
            If conditionalMethods.Length = 1 Then
                conditionalExpr = SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName(conditionalMethods(0)))
            ElseIf conditionalMethods.Length > 1 Then
                conditionalExpr = SyntaxFactory.AndAlsoExpression(SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName(conditionalMethods(0))), SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName(conditionalMethods(1))))
                For i As Integer = 2 To conditionalMethods.Length - 1
                    conditionalExpr = SyntaxFactory.AndAlsoExpression(conditionalExpr, SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName(conditionalMethods(i))))
                Next
            End If
            If hasSetterPreCondMeth Then
                Dim preCondMeth =
                        SyntaxFactory.InvocationExpression(SyntaxFactory.ParseName($"BeforeSet{actualName}")).
                            AddArgumentListArguments({
                                SyntaxFactory.SimpleArgument(SyntaxFactory.IdentifierName("Value")),
                                SyntaxFactory.SimpleArgument(SyntaxFactory.IdentifierName(context.TargetSymbol.Name))
                            })
                If conditionalExpr Is Nothing Then
                    conditionalExpr = preCondMeth
                Else
                    conditionalExpr = SyntaxFactory.AndAlsoExpression(preCondMeth, conditionalExpr)
                End If
            End If
            valueSetStatement = 
                SyntaxFactory.MultiLineIfBlock(
                    SyntaxFactory.IfStatement(
                        conditionalExpr
                    )
                ).AddStatements(valueSetStatement).AddStatements(methodsToCallInvocations.ToArray())
        End If
        Dim propertySetter = 
                SyntaxFactory.SetAccessorBlock(SyntaxFactory.SetAccessorStatement()).
                    AddStatements(valueSetStatement)
        If Not hasAnyPreCond Then
            propertySetter = propertySetter.AddStatements(methodsToCallInvocations.ToArray())
        End If
        Dim propertyBlock =
                SyntaxFactory.PropertyBlock(
                    SyntaxFactory.PropertyStatement(actualName).
                        WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword))).
                        WithAsClause(SyntaxFactory.SimpleAsClause(SyntaxFactory.ParseTypeName(fieldSymbol.Type.Name))),
                    SyntaxFactory.List(Of AccessorBlockSyntax)({propertyGetter, propertySetter})
                )
        Dim typeName = context.TargetSymbol.ContainingType.Name
        Dim classStatement = SyntaxFactory.ClassStatement(typeName).
                AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword), SyntaxFactory.Token(SyntaxKind.PartialKeyword))
        Dim classBlock = SyntaxFactory.ClassBlock(classStatement).AddMembers(propertyBlock)
        Dim result As SyntaxNode = classBlock
        If context.SemanticModel.Compilation.RootNamespace().MetadataName <> context.TargetSymbol.ContainingNamespace.MetadataName Then
            result = SyntaxFactory.NamespaceBlock(SyntaxFactory.NamespaceStatement(SyntaxFactory.ParseName(context.TargetSymbol.ContainingNamespace.MetadataName))).AddMembers(classBlock)
        End If
        Return New PropertyGenInfo(result.NormalizeWhitespace(), $"{fieldSymbol.ContainingType.Name}_{actualName}")
    End Function
    
    Private Shared Function GetPropertyName(name As String) As String
        If name.StartsWith("_") Then
            name = name.Substring(1)
        ElseIf name.StartsWith("m_") Then
            name = name.Substring(2)
            Return name.Remove(0, name.IndexOf(name.First(Function(x) Char.IsUpper(x))))
        End If
        Return $"{Char.ToUpper(name(0))}{name.Remove(0, 1)}"
    End Function
End Class

Public Class PropertyGenInfo
    Sub New(syntax As SyntaxNode, name As String)
        Me.Syntax = syntax
        Me.Name = name
    End Sub

    Public ReadOnly Property Name As String
    Public ReadOnly Property Syntax As SyntaxNode
End Class