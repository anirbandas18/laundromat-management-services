package com.teenthofabud.laundromat.manager.access.operation.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationException;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationForm;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface OperationService {

    public Set<OperationVo> retrieveAllByNaturalOrdering();

    public OperationVo retrieveDetailsById(long id) throws OperationException;

    public List<OperationVo> retrieveAllMatchingDetailsByName(String name) throws OperationException;

    public Long createOperation(OperationForm form) throws OperationException;

    public void updateOperation(Long id, OperationForm form) throws OperationException;

    public void deleteOperation(Long id) throws OperationException;

    public void applyPatchOnOperation(Long id, List<PatchOperationForm> patches) throws OperationException;

}
