export interface AppointmentDTO
{
    operationRequestId: number,
    operationTypeId: string,
    surgeryRoomId: number,
    date: string,
    time: number,
    status: string
}