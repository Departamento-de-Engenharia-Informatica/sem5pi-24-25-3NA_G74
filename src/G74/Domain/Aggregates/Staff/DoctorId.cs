using G74.Domain.Shared;

public class DoctorId : EntityId
{
    public DoctorId(object value) : base(value)
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