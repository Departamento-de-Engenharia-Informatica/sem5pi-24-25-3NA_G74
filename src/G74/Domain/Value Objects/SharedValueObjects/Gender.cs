using G74.Domain.Shared;

namespace G74.Domain.Patient;

public class Gender : IValueObject
{
    private string GenderDescription { get; }

    public enum GenderEnum
    {
        Male,

        Female,

        Other
    }

    public Gender(GenderEnum gender)
    {
        GenderDescription = gender.ToString();
    }
}