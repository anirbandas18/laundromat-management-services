package com.teenthofabud.laundromat.manager.access.resource.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceException;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceForm;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface ResourceService {

    public Set<ResourceVo> retrieveAllByNaturalOrdering();

    public ResourceVo retrieveDetailsById(long id) throws ResourceException;

    public List<ResourceVo> retrieveAllMatchingDetailsByName(String name) throws ResourceException;

    public Long createResource(ResourceForm form) throws ResourceException;

    public void updateResource(Long id, ResourceForm form) throws ResourceException;

    public void deleteResource(Long id) throws ResourceException;

    public void applyPatchOnResource(Long id, List<PatchOperationForm> patches) throws ResourceException;

}
