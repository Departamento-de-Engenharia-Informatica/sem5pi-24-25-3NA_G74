
import mongoose from 'mongoose';
import {IAllergyPersistence} from "../../dataschema/IAllergyPersistence";

const AllergySchema = new mongoose.Schema(
    {
        domainId: { type: String, unique: true },
        code: { type: String, unique: true },
        designation: { type: String },
        description: { type: String },
    },
    { timestamps: true },
);

export default mongoose.model<IAllergyPersistence & mongoose.Document>('Allergy', AllergySchema);
