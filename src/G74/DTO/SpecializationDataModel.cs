using G74.Domain.Aggregates.Specialization;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Specialization;

namespace G74.DTO;

public class SpecializationDataModel : Entity<Guid>
{
    public long Code { get; set; }
    public string Designation { get; set; }
    
    protected SpecializationDataModel() : base(Guid.NewGuid())
    {
        
    }
    public SpecializationDataModel(Specialization specialization) : base(Guid.NewGuid())
    {
        Code = specialization.Code.Value;
        Designation = specialization.Designation.Value;
    }
    
    // Constructor for creating from domain model (e.g., for API response)
    public static SpecializationDataModel FromDomain(Specialization specialization)
    {
        return new SpecializationDataModel
        {
            Code = specialization.Code.Value,
            Designation = specialization.Designation.Value
        };
    }
    
    // Method to convert to domain model with validation
    public static Specialization ToDomain(SpecializationDataModel specializationDataModel)
    {
        return new Specialization(
            new Code(specializationDataModel.Code),  // Validation happens in value objects
            new Designation(specializationDataModel.Designation)
        );
    }
    
    public static IEnumerable<Specialization> ToDomain(IEnumerable<SpecializationDataModel> specializationsDataModel)
    {
        List<Specialization> specializationsDomain = new List<Specialization>();
        foreach(SpecializationDataModel specializationDataModel in specializationsDataModel)
        {
            Specialization specializationDomain = ToDomain(specializationDataModel);
            specializationsDomain.Add(specializationDomain);
        }
        return specializationsDomain.AsEnumerable();
    }
}