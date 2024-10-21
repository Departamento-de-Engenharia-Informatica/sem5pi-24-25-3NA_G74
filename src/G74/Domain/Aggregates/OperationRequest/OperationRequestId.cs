namespace G74.Domain;
using G74.Domain.Shared;
public class OperationRequestId : EntityId
{
    public OperationRequestId(object value) : base(value)
    {
    }

    public override string AsString()
    {
        throw new NotImplementedException();
    }

    protected override object createFromString(string text)
    {
        throw new NotImplementedException();
    }
}