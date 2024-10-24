using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace G74.Migrations
{
    /// <inheritdoc />
    public partial class CreateOperationRequestTable : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropColumn(
                name: "Duration",
                table: "OperationRequests");

            migrationBuilder.RenameColumn(
                name: "Operation Type",
                table: "OperationRequests",
                newName: "Name Operation Type");

            migrationBuilder.AlterColumn<DateTime>(
                name: "DeadLine Date",
                table: "OperationRequests",
                type: "datetime2",
                nullable: false,
                oldClrType: typeof(string),
                oldType: "nvarchar(max)");

            migrationBuilder.AddColumn<int>(
                name: "Days",
                table: "OperationRequests",
                type: "int",
                nullable: false,
                defaultValue: 0);

            migrationBuilder.AddColumn<int>(
                name: "Hours",
                table: "OperationRequests",
                type: "int",
                nullable: false,
                defaultValue: 0);

            migrationBuilder.AddColumn<int>(
                name: "Minutes",
                table: "OperationRequests",
                type: "int",
                nullable: false,
                defaultValue: 0);

            migrationBuilder.AddColumn<int>(
                name: "Seconds",
                table: "OperationRequests",
                type: "int",
                nullable: false,
                defaultValue: 0);
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropColumn(
                name: "Days",
                table: "OperationRequests");

            migrationBuilder.DropColumn(
                name: "Hours",
                table: "OperationRequests");

            migrationBuilder.DropColumn(
                name: "Minutes",
                table: "OperationRequests");

            migrationBuilder.DropColumn(
                name: "Seconds",
                table: "OperationRequests");

            migrationBuilder.RenameColumn(
                name: "Name Operation Type",
                table: "OperationRequests",
                newName: "Operation Type");

            migrationBuilder.AlterColumn<string>(
                name: "DeadLine Date",
                table: "OperationRequests",
                type: "nvarchar(max)",
                nullable: false,
                oldClrType: typeof(DateTime),
                oldType: "datetime2");

            migrationBuilder.AddColumn<string>(
                name: "Duration",
                table: "OperationRequests",
                type: "nvarchar(max)",
                nullable: false,
                defaultValue: "");
        }
    }
}
