export interface Patient {

  name: string;
  gender: string;
  dateOfBirth: {
    yearOfBirth: number;
    monthOfBirth: number;
    dayOfBirth: number;
  };
  contactInformation: {
    phoneNumber: string;
    emailAddress: string;
  };
  emergencyContact: {
    name: string;
    phoneNumber: string;
  };

}