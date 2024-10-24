using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using Newtonsoft.Json;

using G74.Adapters.Repositories;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
public class OperationRequestEntityTypeConfiguration : IEntityTypeConfiguration<DataOperationRequest>
{
    public void Configure(EntityTypeBuilder<DataOperationRequest> builder)
    {
        builder.HasKey(o => o.Id);

        builder.Property(o => o.MedicalRecordNumber)
           .IsRequired()
           .HasColumnName("Medical Record Number");

        builder.Property(o => o.NameOperationType)
            .IsRequired()
            .HasColumnName("Name Operation Type");

        var requiredStaffBySpecialization = builder.Property(o => o.RequiredStaffBySpecialization)
            .IsRequired()
            .HasConversion(new SpecializationStaffListConverter())
            .HasColumnName("Required Staff By Specialization");

        requiredStaffBySpecialization.Metadata.SetValueComparer(new SpecializationStaffListComparer());

         builder.Property(o => o.Seconds)
            .IsRequired()
            .HasColumnName("Seconds");

         builder.Property(o => o.Minutes)
            .IsRequired()
            .HasColumnName("Minutes");

         builder.Property(o => o.Hours)
            .IsRequired()
            .HasColumnName("Hours");

         builder.Property(o => o.Days)
            .IsRequired()
            .HasColumnName("Days");

        builder.Property(o => o.LicenceNumber)
           .IsRequired()
           .HasColumnName("Licence Number");

        builder.Property(o => o.DeadlineDate)
           .IsRequired()
           .HasColumnName("DeadLine Date");
        
        builder.Property(o => o.Priority)
           .IsRequired()
           .HasColumnName("Priority");
         
    }
    
}

public class SpecializationStaffListComparer : ValueComparer<List<SpecializationStaff>>
{
    public SpecializationStaffListComparer() : base(
        (c1, c2) => JsonConvert.SerializeObject(c1) == JsonConvert.SerializeObject(c2),
        c => c.Aggregate(0, (a, v) => HashCode.Combine(a, v.GetHashCode())),
        c => JsonConvert.DeserializeObject<List<SpecializationStaff>>(JsonConvert.SerializeObject(c)))
    {
    }
}

internal class SpecializationStaffListConverter : ValueConverter<List<SpecializationStaff>, string>
{
    public SpecializationStaffListConverter() : base(
        v => JsonConvert.SerializeObject(v),
        v => JsonConvert.DeserializeObject<List<SpecializationStaff>>(v))
    {
    }
}