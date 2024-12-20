import mongoose from 'mongoose';

export interface IMedicalRecordPersistence {
    domainId: string;
    allergies: mongoose.Types.ObjectId[];
    medicalConditions: mongoose.Types.ObjectId[];
    freeText: string;
}
