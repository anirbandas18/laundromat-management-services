package com.teenthofabud.laundromat.manager.tax.lov.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVException;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVForm;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface TaxLOVService {

    public Set<TaxLOVVo> retrieveAllByNaturalOrdering();

    public TaxLOVVo retrieveDetailsById(Long id) throws TaxLOVException;

    public List<TaxLOVVo> retrieveAllMatchingDetailsByName(String name) throws TaxLOVException;

    public Long createTaxLOV(TaxLOVForm form) throws TaxLOVException;

    public void updateTaxLOV(Long id, TaxLOVForm form) throws TaxLOVException;

    public void deleteTaxLOV(Long id) throws TaxLOVException;

    public void applyPatchOnTaxLOV(Long id, List<PatchOperationForm> patches) throws TaxLOVException;

}
