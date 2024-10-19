using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class MedicalCondition : IValueObject
{

    private string MedicalConditionsDescriptions { get; }

    public MedicalCondition(string medicalConditionsDescriptions)
    {
        MedicalConditionsValidations(medicalConditionsDescriptions);

        MedicalConditionsDescriptions = medicalConditionsDescriptions;
    }

    private void MedicalConditionsValidations(string medicalConditionDescription)
    {
        if (!string.IsNullOrWhiteSpace(medicalConditionDescription))
            throw new ArgumentException(MedicalConditionDescriptionNullOrWhiteSpaceMsg);

    }

    private const string MedicalConditionDescriptionNullOrWhiteSpaceMsg = "Medical condition cannot be empty or spaces";
    
    
}