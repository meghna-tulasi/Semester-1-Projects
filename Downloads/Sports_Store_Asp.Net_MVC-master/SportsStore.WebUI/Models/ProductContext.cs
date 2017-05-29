using System;
namespace SportsStore.WebUI
{
	public class ProductContext
	{
		public ProductContext()
		{
			public DbSet<Order> Orders { get; set; }
		public DbSet<OrderDetail> OrderDetails { get; set; }
		public DbSet<Product> Products { get; set; }
		}
	}
}
