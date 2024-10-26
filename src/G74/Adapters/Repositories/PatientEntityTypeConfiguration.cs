using G74.DataModel;
using G74.Infrastructure.Converters;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace G74.Adapters.Repositories;

public class PatientEntityTypeConfiguration : IEntityTypeConfiguration<PatientDataModel>
{
    public void Configure(EntityTypeBuilder<PatientDataModel> builder)
    {
        builder.HasKey(p => p.Id);

        builder.Property(p => p.MedicalRecordNumber)
            .HasConversion(new MedicalRecordNumberConverter())
            .IsRequired()
            .HasColumnName("MedicalRecordNumber");
        builder.Property(p => p.Name)
            .HasConversion(new NameConverter())
            .IsRequired()
            .HasColumnName("Name");
        builder.Property(p => p.DateOfBirth)
            .HasConversion(new DateOfBirthConverter())
            .IsRequired()
            .HasColumnName("DateOfBirth");
        builder.Property(p => p.Gender)
            .HasConversion(new GenderConverter())
            .IsRequired()
            .HasColumnName("Gender");

        builder.Property(p => p.ContactInformation)
            .HasConversion(new ContactInformationConverter())
            .IsRequired()
            .HasColumnName("ContactInformation");

        builder.Property(p => p.EmergencyContact)
            .HasConversion(new EmergencyContactConverter())
            .IsRequired()
            .HasColumnName("EmergencyContact");

        // Configure DeletionInformation as an owned entity
        builder.OwnsOne(p => p.DeletionInformation, deletion =>
        {
            deletion.Property(d => d.ToDelete)
                .IsRequired()
                .HasColumnName("ToDelete");

            deletion.Property(d => d.DateToBeDeleted)
                .HasColumnName("DateToBeDeleted");
        });

        builder.HasIndex(p => p.MedicalRecordNumber).IsUnique();

        
    }
}