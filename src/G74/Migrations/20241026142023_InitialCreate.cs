using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace G74.Migrations
{
    /// <inheritdoc />
    public partial class InitialCreate : Migration
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
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "OperationRequests");
        }
    }
}
