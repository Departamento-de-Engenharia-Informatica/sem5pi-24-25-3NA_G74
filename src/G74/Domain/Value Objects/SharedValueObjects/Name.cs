using G74.Domain.Shared;


namespace G74.Domain.Value_Objects.SharedValueObjects;

public class Name : IValueObject
{
    public string TheName { get; }

    public Name(string name)
    {
        if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException(IsNullOrWithSpaceMsg);

        TheName = name;
    }

    private const string IsNullOrWithSpaceMsg = "Name cannot be empty or spaces";

    public static Name FromString(string nameStr)
    {
        if (string.IsNullOrWhiteSpace(nameStr))
        {
            throw new ArgumentException("Name cannot be empty or spaces.");
        }

        return new Name(nameStr.Trim()); 
    }

    public override string ToString()
    {
        return TheName;
    }
    
    
    
}