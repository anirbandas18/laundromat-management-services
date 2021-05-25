package com.teenthofabud.laundromat.manager.type.lov.service;

import com.teenthofabud.core.common.model.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.error.TypeException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface TypeLOVService {

    public Set<TypeLOVVo> retrieveAllByNaturalOrdering();

    public TypeLOVVo retrieveDetailsById(long id) throws TypeException;

    public List<TypeLOVVo> retrieveAllMatchingDetailsByName(String name) throws TypeException;

    public Long createTypeLOV(TypeLOVForm form) throws TypeException;

    public void updateTypeLOV(Long id, TypeLOVForm form) throws TypeException;

    public void deleteTypeLOV(Long id) throws TypeException;

    public void applyPatchOnTypeLOV(Long id, List<PatchOperationForm> patches) throws TypeException;



}
