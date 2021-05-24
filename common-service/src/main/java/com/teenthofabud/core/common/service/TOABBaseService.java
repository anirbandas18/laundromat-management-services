package com.teenthofabud.core.common.service;

import com.teenthofabud.core.common.model.error.TOABBaseException;
import com.teenthofabud.core.common.model.form.PatchOperationForm;

import java.util.List;

public interface TOABBaseService {

    public void validatePatches(List<PatchOperationForm> patches, String domain) throws TOABBaseException;

}
