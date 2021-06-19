package com.teenthofabud.laundromat.manager.type.model.service;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelException;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface TypeModelService {

    public Set<TypeModelVo> retrieveAllByNaturalOrdering();

    public TypeModelVo retrieveDetailsById(long id) throws TypeModelException;

    public List<TypeModelVo> retrieveDetailsByTypeLOVId(long typeLovId) throws TypeModelException;

    public List<TypeModelVo> retrieveAllMatchingDetailsByName(String name) throws TypeModelException;

    public Long createTypeModel(TypeModelForm form) throws TypeModelException;

    public void updateTypeModel(Long id, TypeModelForm form) throws TypeModelException;

    public void deleteTypeModel(Long id) throws TypeModelException;

    public void applyPatchOnTypeModel(Long id, List<PatchOperationForm> patches) throws TOABBaseException;



}
