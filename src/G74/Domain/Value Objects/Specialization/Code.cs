using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Specialization;

public class Code : IValueObject
{
    public long Value { get; }

    public Code(long value)
    {
        this.Value = value;
    }
    public Code(string code)
    {
        try
        {
            this.Value = long.Parse(code);
        }
        catch (System.Exception)
        {
            throw new System.ArgumentException("Invalid Code");
        }
    
    }

    public override string ToString()
    {
        return Value.ToString();
    }

}