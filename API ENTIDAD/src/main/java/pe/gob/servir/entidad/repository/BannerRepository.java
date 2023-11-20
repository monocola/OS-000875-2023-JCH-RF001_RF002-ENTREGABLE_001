package pe.gob.servir.entidad.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import pe.gob.servir.entidad.model.Banner;

public interface BannerRepository extends JpaRepository<Banner , Long>{

	
	@Query("select b from Banner b where b.estadoRegistro='1' order by b.orden asc")
	public List<Banner> obtenerBanners();
	
	@Query("select count(b) from Banner b where b.estadoRegistro='1'")
	public Long obtenerOrden();
}
