using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace G74.Migrations
{
    /// <inheritdoc />
    public partial class ChangeLicenseNumberToString : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "OperationRequests",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    MedicalRecordNumber = table.Column<string>(name: "Medical Record Number", type: "nvarchar(max)", nullable: false),
                    LicenceNumber = table.Column<string>(name: "Licence Number", type: "nvarchar(max)", nullable: false),
                    NameOperationType = table.Column<string>(name: "Name Operation Type", type: "nvarchar(max)", nullable: false),
                    RequiredStaffBySpecialization = table.Column<string>(name: "Required Staff By Specialization", type: "nvarchar(max)", nullable: false),
                    Seconds = table.Column<int>(type: "int", nullable: false),
                    Minutes = table.Column<int>(type: "int", nullable: false),
                    Hours = table.Column<int>(type: "int", nullable: false),
                    Days = table.Column<int>(type: "int", nullable: false),
                    DeadLineDate = table.Column<DateTime>(name: "DeadLine Date", type: "datetime2", nullable: false),
                    Priority = table.Column<string>(type: "nvarchar(max)", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_OperationRequests", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Patients",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    PatientName = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    MedicalRecordNumber = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    PatientGender = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    BirthDate = table.Column<DateOnly>(type: "date", nullable: false),
                    PersonalPhoneNumber = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    PersonalEmail = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    EmergencyContactName = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    EmergencyContactPhoneNumber = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    MarkedForDeletion = table.Column<bool>(type: "bit", nullable: false),
                    DateToBeDeleted = table.Column<DateTime>(type: "datetime2", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Patients", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Staff",
                columns: table => new
                {
                    Id = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Name = table.Column<string>(type: "nvarchar(100)", maxLength: 100, nullable: false),
                    PhoneNumber = table.Column<string>(type: "nvarchar(20)", maxLength: 20, nullable: false),
                    ContactEmail = table.Column<string>(type: "nvarchar(100)", maxLength: 100, nullable: false),
                    StaffSpecialization = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                    Status = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Staff", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Users",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    Username = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                    Email = table.Column<string>(type: "nvarchar(100)", maxLength: 100, nullable: false),
                    Role = table.Column<string>(type: "nvarchar(20)", maxLength: 20, nullable: false),
                    IsMarkedForDeletion = table.Column<bool>(type: "bit", nullable: false),
                    DateToBeDeleted = table.Column<DateTime>(type: "datetime2", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Users", x => x.Id);
                });
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "OperationRequests");

            migrationBuilder.DropTable(
                name: "Patients");

            migrationBuilder.DropTable(
                name: "Staff");

            migrationBuilder.DropTable(
                name: "Users");
        }
    }
}
