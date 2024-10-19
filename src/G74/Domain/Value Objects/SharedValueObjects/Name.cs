using G74.Domain.Shared;


namespace G74.Domain.Value_Objects.SharedValueObjects;

public class Name : IValueObject
{
    public String TheName { get; }

    public Name(String name)
    {
        if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException(IsNullOrWithSpaceMsg);

        TheName = name;
    }

    private const string IsNullOrWithSpaceMsg = "Name cannot be empty or spaces";
}