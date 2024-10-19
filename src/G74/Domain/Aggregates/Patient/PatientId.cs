using G74.Domain.Shared;

namespace G74.Domain.Patient;

public class PatientId : EntityId
{
    public PatientId(Guid value) : base(value)
    {
    }

    public PatientId(string value) : base(value)
    {
    }

    protected override object createFromString(string id)
    {
        return new Guid(id);
    }

    public override String AsString()
    {
        Guid obj = (Guid) ObjValue;
        return obj.ToString();
    }
    
    public Guid AsGuid()
    {
        return (Guid) ObjValue;
    }
}