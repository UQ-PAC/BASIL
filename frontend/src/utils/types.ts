// src/utils/types.ts

export interface DatasetConfig {
    adt: string;
    relf: string;
}

export const getDatasetName = (path: string): string => {
    const condensedPath = path.replace("src/test/correct/", '');
    return condensedPath.replace('.adt', '');
};