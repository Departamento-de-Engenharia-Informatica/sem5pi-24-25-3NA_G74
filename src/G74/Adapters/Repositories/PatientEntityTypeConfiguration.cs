using G74.DataModel;
using G74.Domain;
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
        
        
        


    }
    /*
    Name = patient.Name;
    MedicalRecordNumber = patient.MedicalRecordNumber;
    DateOfBirth = patient.DateOfBirth;
    Gender = patient.Gender;
    ContactInformation = patient.ContactInformation;
    EmergencyContact = patient.EmergencyContact;
    AppointmentHistory = patient.AppointmentHistory;
    MedicalCondition = patient.MedicalCondition;
    */
    
}