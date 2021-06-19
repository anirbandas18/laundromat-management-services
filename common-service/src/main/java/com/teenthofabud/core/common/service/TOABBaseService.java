package com.teenthofabud.core.common.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABSystemException;

import java.util.List;

public interface TOABBaseService {

    public void validatePatches(List<PatchOperationForm> patches, String domain) throws TOABSystemException;

}
