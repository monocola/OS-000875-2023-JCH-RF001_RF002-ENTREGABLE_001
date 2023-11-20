package pe.gob.servir.mensajeria.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import feign.Param;
import pe.gob.servir.mensajeria.model.PlantillaCorreo;

public interface PlantillaCorreoRepository extends JpaRepository<PlantillaCorreo, Long>{
	
	@Query("SELECT p FROM PlantillaCorreo p WHERE p.estadoRegistro='1' AND p.codigoPlantilla =:codPlantilla")
	public PlantillaCorreo obtenerPlantilla(@Param("codPlantilla")String codPlantilla);

}
