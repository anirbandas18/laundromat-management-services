package com.teenthofabud.laundromat.manager.tax.model.service;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelException;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface TaxModelService {

    public Set<TaxModelVo> retrieveAllByNaturalOrdering();

    public TaxModelVo retrieveDetailsById(Long id) throws TaxModelException;

    public List<TaxModelVo> retrieveDetailsByTaxLov(Long taxLovId) throws TaxModelException;

    public List<TaxModelVo> retrieveDetailsByCurrencyTypeModel(Long currencyTypeModelId) throws TaxModelException;

    public List<TaxModelVo> retrieveAllMatchingDetailsByName(String name) throws TaxModelException;

    public Long createTaxModel(TaxModelForm form) throws TaxModelException;

    public void updateTaxModel(Long id, TaxModelForm form) throws TaxModelException;

    public void deleteTaxModel(Long id) throws TaxModelException;

    public void applyPatchOnTaxModel(Long id, List<PatchOperationForm> patches) throws TOABBaseException;



}
