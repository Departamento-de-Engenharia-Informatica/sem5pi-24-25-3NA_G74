namespace G74.Domain.Value_Objects;

public class Username
{
    public string name { get; }

    public Username(string username)
    {
        name = username;
    }
    
    public override bool Equals(object obj)
    {
        if (obj is Username otherUsername)
        {
            return name == otherUsername.name;
        }
        return false;
    }

    public override int GetHashCode()
    {
        return name.GetHashCode();
    }

    public override string ToString()
    {
        return name;
    }

    public static bool VerifyUsername(string username)
    {
        return !string.IsNullOrWhiteSpace(username);
    }
    
}