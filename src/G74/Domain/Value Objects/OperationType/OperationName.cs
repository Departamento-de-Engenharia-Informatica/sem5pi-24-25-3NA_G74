using G74.Domain.Shared;

public class OperationName : IValueObject
{
    public string name { get; private set; }

    public OperationName(string name){
        this.name = name;
    }
}