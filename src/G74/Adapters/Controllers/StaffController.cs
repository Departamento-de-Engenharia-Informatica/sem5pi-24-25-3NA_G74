using System.ComponentModel.DataAnnotations;
using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;

[Route("api/[controller]")]
[ApiController]
public class StaffController : ControllerBase
{
    private readonly StaffAppService _staffAppService;
    private readonly StaffToDto _staffToDto;
    
    public StaffController(StaffAppService staffAppService, StaffToDto staffToDto)
    {
        _staffAppService = staffAppService;
        _staffToDto = staffToDto;
    }
    
    // GET: api/Staff/
    [HttpGet]
    public async Task<ActionResult<StaffDto>> GetStaff()
    {
        IEnumerable<StaffDto> staffDto = await _staffAppService.GetAll();

        return Ok(staffDto);
    }
    
    // GET: api/Staff//license/682468
    [HttpGet("license/{licenseNumber}")]
    public async Task<ActionResult<StaffDto>> GetStaffByLicenseNumber(string licenseNumber)
    {
        var staffDTO = await _staffAppService.GetByLicenseNumber(licenseNumber);

        if (staffDTO == null)
        {
            return NotFound();
        }
        return Ok(staffDTO);
    }
    
    // POST: api/Staff
    // To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
    [HttpPost]
    public async Task<ActionResult<StaffDto>> RegisterStaff([FromBody]JsonStaffDto jsonStaffDto)
    {
        try
        {
            StaffDto staffDto = _staffToDto.JsonToDto(jsonStaffDto);
            StaffDto resultStaffDto = await _staffAppService.Add(staffDto);
            
            return CreatedAtAction(
                nameof(GetStaffByLicenseNumber),
                new { licenseNumber = resultStaffDto.LicenseNumber },
                resultStaffDto);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }
    
    // PUT: api/Staff/license/{licenseNumber}
    [HttpPut("license/{licenseNumber}")]
    public async Task<ActionResult<StaffDto>> UpdateStaff(string licenseNumber, [FromBody] JsonStaffDto jsonStaffDto)
    {
        try
        {
            // maybe put this in service instead and just catch exception here instead
            StaffDto? existingStaff = await _staffAppService.GetByLicenseNumber(licenseNumber);
            if (existingStaff == null)
            {
                return NotFound($"Staff with license number {licenseNumber} not found.");
            }
            StaffDto staffDto = _staffToDto.JsonToDto(jsonStaffDto);
            var staffDtoResult = await _staffAppService.Update(licenseNumber, staffDto);
            
            return Ok(staffDtoResult);
        }
        catch (ValidationException ex)
        {
            return BadRequest(ex.Message);
        }
        // catch (Exception ex)
        // {
        //     // return StatusCode(500, "An error occurred while updating the staff profile.");
        //     return StatusCode(500, ex.Message);
        // }
    }
    
    [HttpPatch("license/{licenseNumber}/deactivate")]
    public async Task<ActionResult<StaffDto>> DeactivateStaff(string licenseNumber)
    {
        try
        {
            var staffDto = await _staffAppService.Deactivate(licenseNumber);
            if (staffDto == null)
            {
                return NotFound($"Staff with license number {licenseNumber} not found.");
            }
            return Ok(staffDto);
        }
        catch (ValidationException ex)
        {
            return BadRequest(ex.Message);
        }
    }
}