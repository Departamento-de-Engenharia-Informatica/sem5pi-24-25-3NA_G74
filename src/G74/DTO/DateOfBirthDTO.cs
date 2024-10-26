namespace G74.DTO;

public class DateOfBirthDTO
{
    public int YearOfBirth { get; set; }

    public int MonthOfBirth { get; set; }

    public int DayOfBirth { get; set; }
    
    public DateOfBirthDTO() { }

    public DateOfBirthDTO(int yearOfBirth, int monthOfBirth, int dayOfBirth)
    {
        YearOfBirth = yearOfBirth;
        MonthOfBirth = monthOfBirth;
        DayOfBirth = dayOfBirth;
    }
}