using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class MedicalCondition : IValueObject
{
    public string Description { get; }

    private const string MedicalConditionDescriptionNullOrWhiteSpaceMsg = "Medical condition cannot be empty or spaces";

    public MedicalCondition(string medicalConditionsDescriptions)
    {
        Description = MedicalConditionsValidations(medicalConditionsDescriptions);
    }

    private static string MedicalConditionsValidations(string medicalConditionDescription)
    {
        if (string.IsNullOrWhiteSpace(medicalConditionDescription))
            throw new BusinessRuleValidationException(MedicalConditionDescriptionNullOrWhiteSpaceMsg);

        return medicalConditionDescription.Trim();
    }

    public override string ToString() => Description;

    public bool Equals(MedicalCondition? other) => other != null && Description == other.Description;

    public override bool Equals(object? obj) => obj is MedicalCondition other && Equals(other);

    public override int GetHashCode() => Description.GetHashCode();
}