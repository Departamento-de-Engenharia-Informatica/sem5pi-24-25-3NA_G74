using G74.Domain.Shared;

public class SpecializationStaff : IValueObject
{
    public string Specialization {get; set; }

    public SpecializationStaff(string specializationStaff){
        Specialization = specializationStaff;
    }

}