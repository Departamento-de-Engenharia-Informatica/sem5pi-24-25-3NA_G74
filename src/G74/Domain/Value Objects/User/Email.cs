using System.Text.RegularExpressions;

namespace G74.Domain.Value_Objects;

public class Email
{
    private string email { get; set; }

    public Email(string email)
    {
        if (!IsValidEmail(email))
        {
            throw new ArgumentException("Invalid email format.");
        }
        this.email = email ?? throw new ArgumentNullException(nameof(email));
    }

    public static bool IsValidEmail(string email)
    {
        var emailPattern = @"^[^@\s]+@[^@\s]+\.[^@\s]+$";
        return Regex.IsMatch(email, emailPattern);
    }
    
    public override bool Equals(object obj)
    {
        if (obj is Email otherEmail)
        {
            return email == otherEmail.email;
        }
        return false;
    }

    public override int GetHashCode()
    {
        return email.GetHashCode();
    }

    public override string ToString()
    {
        return email;
    }
}