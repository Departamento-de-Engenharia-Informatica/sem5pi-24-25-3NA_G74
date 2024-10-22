using G74.Domain;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class PatientEntityTypeConfiguration : IEntityTypeConfiguration<DataPatient>
{
    public void Configure(EntityTypeBuilder<DataPatient> builder)
    {
        builder.HasKey(u => u.Id);

        builder.Property(u => u.Name)
            .HasMaxLength(50)
            .IsRequired();

        builder.Property(u => u.Gender);

        builder.Property(u => u.DateOfBirth);

        builder.Property(u => u.ContactInformation);

        builder.Property(u => u.EmergencyContact);

    }
    /**
    public Guid Id { get; set; }
    public string Name { get; set; }
    public string Gender { get; set; }
    public string DateOfBirth { get; set; }
    public string ContactInformation { get; set; }
    public string EmergencyContact { get; set; }
    */
    
}